package kingscross

import java.util.concurrent.atomic._
import jep._
import scala.annotation._
import scala.collection._
import scala.language.dynamics

/**
 * Wraps a `Jep` Python interpreter.
 * @author Tongfei Chen
 */
class Python private[kingscross](val jep: Jep) extends Interpreter { python =>

  private[this] val pyObjectId = new AtomicInteger(0)

  class Expr private[kingscross](val py: String) extends common.Expr[python.type] with Dynamic {
    val lang: python.type = python
    def expr = py

    def applyDynamic(method: String)(params: Expr*) = new Expr(
      s"($py).$method(${params.map(_.py).mkString(", ")})"
    )

    def applyDynamicNamed(method: String)(params: (String, Expr)*) = new Expr(
      s"($py).$method(${params.map { case (k, w) => s"$k=${w.py}"}.mkString(", ")})"
    )

    def selectDynamic(field: String) = new Expr(s"($py).$field")
    def updateDynamic(field: String, value: Expr) = new Expr(s"($py).$field = ${value.py}").!

  }

  class Ref private[kingscross](py: String) extends Expr(py) with common.Ref[python.type] with Dynamic {
    override def finalize() = jep eval s"del $py"
    override def toString() = jep.getValue(s"str($py)").asInstanceOf[String]
  }

  class Package private[kingscross](name: String) extends common.Package[python.type] with Dynamic {
    def applyDynamic(method: String)(params: Expr*) = {
      if (method == "apply")
        newRef(s"$name(${params.map(_.py).mkString(", ")})")
      else
        newRef(s"$name.$method(${params.map(_.py).mkString(", ")})")
    }
    def selectDynamic(field: String) = new Expr(s"$name.$field")
    def updateDynamic(field: String)(value: Expr) = new Expr(s"$name.$field = ${value.py}").!!
  }

  class Function private[kingscross](val args: Seq[String], val name: String) {
    def self = newExpr(name)
  }

  object Global extends Dynamic {
    py"import sys".!
    val PythonVersion = py"sys.version_info[0]".!!.toScala[String](Unmarshaller.string)

    def applyDynamic(method: String)(params: Expr*) =
      new Expr(s"$method(${params.map(_.py).mkString(", ")})")

    def selectDynamic(name: String) = new Expr(name)
  }

  def newId = {
    val id = s"__$pyObjectId"
    pyObjectId.incrementAndGet()
    id
  }

  def newExpr(s: String) = new Expr(s)

  def newRef(s: String, n: String) = {
    jep.eval(s"$n = $s")
    new Ref(n)
  }

  def getRef(n: String) = new Ref(n)

  def fromJvm(x: AnyRef): Ref = {
    val id = newId
    setRaw(id, x)
    new Ref(id)
  }

  def setRaw(s: String, v: Any) = jep.set(s, v)
  def getRaw(s: String): Any = jep getValue s
  def exec(s: String) = jep eval s

  def loadPackage(s: String) = {
    jep eval s"import $s"
    new Package(s)
  }

  def defineFunction(args: String*)(body: String) = {
    val id = newId
    val pyDef1 = s"def $id(${args.mkString(", ")}):"
    val pyDefs = body.split("\n").map(l => "    " + l).mkString("\n")
    val pyDef = s"$pyDef1\n$pyDefs\n"
    exec(pyDef)
    new Function(args, id)
  }

  //region Syntactic sugars

  implicit def toPython[A](a: A)(implicit m: Marshaller[A]) = m marshall a

  implicit class ToPython[A](val a: A) {
    // use-site contravariance
    /** Converts this object to its corresponding representation in Python. */
    def toPython[B >: A](implicit m: Marshaller[B]) = m marshall a
  }

  implicit class ToScala(val o: Expr) {
    /** Converts this object to its corresponding representation in Scala. */
    def toScala[A](implicit u: Unmarshaller[A]) = u unmarshall o
  }

  implicit class PythonInterpolator(val sc: StringContext) {
    /**
     * Python string interpolator: executes Python script in Scala through Jep.
     * Automatically trims excessive space in the start of the lines.
     */
    def py(args: Expr*): Expr = {
      val body = sc.s(args.map {
        case arg: Ref => arg.py
        case arg      => s"(${arg.py})"
      }: _*)
      val lines = body.split("\n").filterNot(_.trim == "")
      val indent = lines.map(_.indexWhere(_ != ' ')).min
      val r = lines map { _ drop indent } mkString "\n"
      new Expr(r)
    }
  }

  //endregion

  //region Marshalling

  @implicitNotFound("Cannot marshall a Scala type ${T} to a Python representation.")
  trait Marshaller[T] extends common.Marshaller[python.type, T] {
    //TODO: Should be contravariant, blocking on SI-2509
    def marshall(x: T): Expr
    val lang: python.type = python
  }

  object Marshaller extends LowerPriorityMarshallers {

    implicit val int = New[Int] { x => new Expr(s"$x") }
    implicit val long = New[Long] { x => new Expr(s"${x}l") }
    implicit val double = New[Double] { x => new Expr(s"$x") }
    implicit val float = New[Float] { x => new Expr(s"$x") }
    implicit val boolean = New[Boolean] { x => new Expr(if (x) "True" else "False") }
    implicit val char = New[Char] { x => new Expr(s"'$x'") }
    implicit val charSequence = New[CharSequence] { x => fromJvm(x.toString) }
    implicit val string = New[String] { x => fromJvm(x) }
    implicit val expr = New[Expr] { x => x }
    implicit val ref = New[Ref] { x => x }
    implicit def product1[A: Marshaller] = New[Product1[A]] { x =>
      new Expr(s"(${x._1.expr},)")
    }
    implicit def product2[A: Marshaller, B: Marshaller] = New[Product2[A, B]] { x =>
      new Expr(s"(${x._1.expr}, ${x._2.expr})")
    }
    implicit def product3[A: Marshaller, B: Marshaller, C: Marshaller] = New[Product3[A, B, C]] { x =>
      new Expr(s"(${x._1.expr}, ${x._2.expr}, ${x._3.expr})")
    }
    implicit def tuple1[A: Marshaller] = New[Tuple1[A]] { x =>
      new Expr(s"(${x._1.expr},)")
    }
    implicit def tuple2[A: Marshaller, B: Marshaller] = New[Tuple2[A, B]] { x =>
      new Expr(s"(${x._1.expr}, ${x._2.expr})")
    }
    implicit def tuple3[A: Marshaller, B: Marshaller, C: Marshaller] = New[Tuple3[A, B, C]] { x =>
      new Expr(s"(${x._1.expr}, ${x._2.expr}, ${x._3.expr})")
    }
    implicit def seq[T: Marshaller] = New[Seq[T]] {
      case x: PyList[T] => x.obj
      case x =>
        val pyList = newRef("[]")
        for (e <- x)
          exec(s"${pyList.py}.append(${e.expr})")
        pyList
    }

    implicit def set[T: Marshaller] = New[Set[T]] {
      case x: PySet[T] => x.obj
      case x =>
        val pySet = newRef("set()")
        for (e <- x)
          exec(s"${pySet.py}.add(${e.py})")
        pySet
    }

    implicit def map[K: Marshaller, V: Marshaller] = New[Map[K, V]] {
      case x: PyDict[K, V] => x.obj
      case x =>
        val pyDict = newRef("{}")
        for ((k, v) <- x) {
          exec(s"${pyDict.py}[${k.py}] = ${v.py}")
        }
        pyDict
    }

  }

  trait LowerPriorityMarshallers extends EvenLowerPriorityMarshallers {

    class Function0Runner[+O: Marshaller](o: Ref, f: () => O) {
      def run() = {
        val r = f().toPython
        py"$o = $r".!
      }
    }

    class Function1Runner[-A: Unmarshaller, +O: Marshaller](a: Ref, o: Ref, f: A => O) {
      def run() = {
        val r = f(a.toScala[A]).toPython
        py"$o = $r".!
      }
    }

    class Function2Runner[-A: Unmarshaller, -B: Unmarshaller, +O: Marshaller](a: Ref, b: Ref, o: Ref, f: (A, B) => O) {
      def run() = {
        val r = f(a.toScala[A], b.toScala[B]).toPython
        py"$o = $r".!
      }
    }

    class Function3Runner[-A: Unmarshaller, -B: Unmarshaller, -C: Unmarshaller, +O: Marshaller](a: Ref, b: Ref, c: Ref, o: Ref, f: (A, B, C) => O) {
      def run() = {
        val r = f(a.toScala[A], b.toScala[B], c.toScala[C]).toPython
        py"$o = $r".!
      }
    }

    implicit def function0[O: Marshaller] = New[() => O] { x =>
      val o = newRef("None")
      val f = fromJvm(new Function0Runner(o, x))
      val pyFunc = py"""
        $f.run()
        return $o
      """
      defineFunction()(pyFunc.py).self
    }

    implicit def function1[A: Unmarshaller, O: Marshaller] = New[A => O] { x =>
      val a = newRef("None") // create input placeholder
      val o = newRef("None") // create output placeholder
      val f = fromJvm(new Function1Runner(a, o, x))
      val pyFunc = py"""
        global $a
        $a = _arg1
        $f.run()
        return $o
      """ // write to input placeholder, run, and then return output placeholder
      defineFunction("_arg1")(pyFunc.py).self
    }

    implicit def function2[A: Unmarshaller, B: Unmarshaller, O: Marshaller] = New[(A, B) => O] { x =>
      val a = newRef("None")
      val b = newRef("None")
      val o = newRef("None")
      val f = fromJvm(new Function2Runner(a, b, o, x))
      val pyFunc = py"""
        global $a
        global $b
        $a = _arg1
        $b = _arg2
        $f.run()
        return $o
      """
      defineFunction("_arg1", "_arg2")(pyFunc.py).self
    }

    implicit def function3[A: Unmarshaller, B: Unmarshaller, C: Unmarshaller, O: Marshaller] = New[(A, B, C) => O] { x =>
      val a = newRef("None")
      val b = newRef("None")
      val c = newRef("None")
      val o = newRef("None")
      val f = fromJvm(new Function3Runner(a, b, c, o, x))
      val pyFunc = py"""
        global $a
        global $b
        global $c
        $a = _arg1
        $b = _arg2
        $c = _arg3
        $f.run()
        return $o
      """
      defineFunction("_arg1", "_arg2", "_arg3")(pyFunc.py).self
    }
  }

  trait EvenLowerPriorityMarshallers {
    private[kingscross] case class New[T](f: T => Expr) extends Marshaller[T] {
      def marshall(x: T) = f(x)
    }

    implicit val anyRef = New[AnyRef] { x => fromJvm(x) }
  }

  //endregion

  //region Unmarshalling

  @implicitNotFound("Cannot marshall a Python object to a Scala representation of type ${T}.")
  abstract class Unmarshaller[T] extends common.Unmarshaller[python.type, T] {
    def unmarshall(x: Expr): T
    val lang: python.type = python
  }

  object Unmarshaller {
    private[kingscross] case class New[T](f: Expr => T) extends Unmarshaller[T] {
      def unmarshall(x: Expr) = f(x)
    }

    implicit val int = New[Int] { x => getRaw(x.!!.py).asInstanceOf[Int] }
    implicit val long = New[Long] { x => getRaw(x.!!.py).asInstanceOf[Long] }
    implicit val double = New[Double] { x => getRaw(x.!!.py).asInstanceOf[Double] }
    implicit val float = New[Float] { x => getRaw(x.!!.py).asInstanceOf[Float] }
    implicit val boolean = New[Boolean] { x => getRaw(x.!!.py).asInstanceOf[Boolean] }
    implicit val string = New[String] { x => getRaw(x.!!.py).toString }

    implicit def function0[O: Unmarshaller] = New[() => O] {
      x => () => x.__call__().toScala[O]
    }

    implicit def function1[A: Marshaller, O: Unmarshaller] = New[A => O] {
      x => a => x.__call__(a.toPython).toScala[O]
    }

    implicit def function2[A: Marshaller, B: Marshaller, O: Unmarshaller] = New[(A, B) => O] {
      x => (a, b) => x.__call__(a.toPython, b.toPython).toScala[O]
    }

    implicit def function3[A: Marshaller, B: Marshaller, C: Marshaller, O: Unmarshaller] = New[(A, B, C) => O] {
      x => (a, b, c) => x.__call__(a.toPython, b.toPython, c.toPython).toScala[O]
    }

    implicit def tuple1[A: Unmarshaller] = New[Tuple1[A]] {
      x => Tuple1(x.__getitem__(0).toScala[A])
    }

    implicit def tuple2[A: Unmarshaller, B: Unmarshaller] = New[Tuple2[A, B]] {
      x => Tuple2(x.__getitem__(0).toScala[A], x.__getitem__(1).toScala[B])
    }

    implicit def tuple3[A: Unmarshaller, B: Unmarshaller, C: Unmarshaller] = New[Tuple3[A, B, C]] {
      x => Tuple3(x.__getitem__(0).toScala[A], x.__getitem__(1).toScala[B], x.__getitem__(2).toScala[C])
    }

    implicit def iterator[A: Unmarshaller] = New[PyIterator[A]] {
      x => new PyIterator[A](x.!!)
    }

    implicit def list[A: Marshaller : Unmarshaller] = New[PyList[A]] {
      x => new PyList[A](x.!!)
    }

    implicit def set[A: Marshaller : Unmarshaller] = New[PySet[A]] {
      x => new PySet[A](x.!!)
    }

    implicit def dict[K: Marshaller : Unmarshaller, V: Marshaller : Unmarshaller] = New[PyDict[K, V]] {
      x => new PyDict[K, V](x.!!)
    }
  }

  //endregion

  //region Data structures wrapper
  class PyIterator[T: Unmarshaller](val obj: Ref) extends AbstractIterator[T] {

    private[this] var elem: T = null.asInstanceOf[T]

    def hasNext = {
      if (elem != null) true
      else {
        try {
          val pyElem = newRef(s"next(${obj.py})")
          elem = pyElem.toScala[T]
          elem != null
        }
        catch {
          case _: Exception => false // StopIteration
        }
      }
    }

    def next() = {
      if ((elem != null) || hasNext) {
        val r = elem
        elem = null.asInstanceOf[T]
        r
      }
      else throw new NoSuchElementException
    }
  }

  class PyList[T: Marshaller : Unmarshaller](val obj: Ref) extends mutable.AbstractSeq[T] {
    override def stringPrefix = "py.list"
    def length = Global.len(obj)._get.asInstanceOf[Int]
    def apply(idx: Int) = obj.__getitem__(newExpr(idx.toString)).toScala[T]
    def iterator = new PyIterator[T](newRef(s"iter(${obj.py})"))
    def update(idx: Int, elem: T) = obj.__setitem__(newExpr(idx.toString))(elem.toPython).!
    def +=(elem: T) = {
      obj.append(elem).!
      this
    }
  }

  class PySet[T: Marshaller : Unmarshaller](val obj: Ref) extends mutable.AbstractSet[T] {
    override def stringPrefix = "py.set"
    def contains(elem: T) = newRef(s"(${elem.py}) in ${obj.py}").toScala[Boolean]
    def +=(elem: T) = {
      exec(s"${obj.py}.add(${elem.py})")
      this
    }
    def -=(elem: T) = {
      exec(s"${obj.py}.remove(${elem.py})")
      this
    }
    def iterator = new PyIterator[T](newRef(s"iter(${obj.py})"))
  }

  class PyDict[K: Marshaller : Unmarshaller, V: Marshaller : Unmarshaller](val obj: Ref) extends mutable.AbstractMap[K, V] {
    override def stringPrefix = "py.dict"
    def get(key: K) = try {
      Some(obj.__getitem__(key).toScala[V])
    } catch {
      case e: Exception => None
    }
    override def apply(key: K) = obj.__getitem__(key).toScala[V]
    def iterator = Global.PythonVersion match {
      case "2" => new PyIterator[(K, V)](newRef(s"${obj.py}.iteritems()"))
      case "3" => new PyIterator[(K, V)](newRef(s"${obj.py}.items()"))
    }
    def +=(kv: (K, V)) = {
      obj.__setitem__(kv._1, kv._2).!
      this
    }
    def -=(key: K) = {
      obj.pop(key).!
      this
    }
  }
  //endregion
}

object Python {

  def apply() = new Python(new Jep())

  def withSharedModules(modules: String*) = new Python(new Jep(new JepConfig().addSharedModules(modules: _*)))

}
