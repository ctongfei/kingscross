package kingscross.python

import kingscross._
import kingscross.python.syntax._
import scala.annotation._
import scala.collection._

/**
 * @author Tongfei Chen
 */

@implicitNotFound("Cannot marshall a Scala type ${T} to a Python representation.")
trait Marshaller[T] {
  //TODO: Should be contravariant, blocking on SI-2509
  def marshall(x: T)(implicit python: Python): Expr
}

object Marshaller extends LowerPriorityMarshallers {

  implicit val int = New[Int] { (x, p) => new Expr(s"$x") }
  implicit val long = New[Long] { (x, p) => new Expr(s"${x}l") }
  implicit val double = New[Double] { (x, p) => new Expr(s"$x") }
  implicit val float = New[Float] { (x, p) => new Expr(s"$x") }
  implicit val boolean = New[Boolean] { (x, p) => new Expr(if (x) "True" else "False") }
  implicit val char = New[Char] { (x, p) => new Expr(s"'$x'") }
  implicit val charSequence = New[CharSequence] { (x, p) => p.fromJvm(x.toString) }
  implicit val string = New[String] { (x, p) => p.fromJvm(x) }
  implicit val expr = New[Expr] { (x, p) => x }
  implicit val ref = New[Ref] { (x, p) => x }
  implicit def product1[A: Marshaller] = New[Product1[A]] { (x, p) =>
    implicit val python = p
    new Expr(s"(${x._1.expr},)")
  }
  implicit def product2[A: Marshaller, B: Marshaller] = New[Product2[A, B]] { (x, p) =>
    implicit val python = p
    new Expr(s"(${x._1.expr}, ${x._2.expr})")
  }
  implicit def product3[A: Marshaller, B: Marshaller, C: Marshaller] = New[Product3[A, B, C]] { (x, p) =>
    implicit val python = p
    new Expr(s"(${x._1.expr}, ${x._2.expr}, ${x._3.expr})")
  }
  implicit def tuple1[A: Marshaller] = New[Tuple1[A]] { (x, p) =>
    implicit val python = p
    new Expr(s"(${x._1.expr},)")
  }
  implicit def tuple2[A: Marshaller, B: Marshaller] = New[Tuple2[A, B]] { (x, p) =>
    implicit val python = p
    new Expr(s"(${x._1.expr}, ${x._2.expr})")
  }
  implicit def tuple3[A: Marshaller, B: Marshaller, C: Marshaller] = New[Tuple3[A, B, C]] { (x, p) =>
    implicit val python = p
    new Expr(s"(${x._1.expr}, ${x._2.expr}, ${x._3.expr})")
  }
  implicit def seq[T: Marshaller] = New[Seq[T]] { (x, p) =>
    implicit val python = p
    x match {
      case x: PyList[T] => x.obj
      case x =>
        val pyList = p.newRef("[]")
        for (e <- x)
          p.exec(s"${pyList.py}.append(${e.expr})")
        pyList
    }
  }


  implicit def set[T: Marshaller] = New[Set[T]] { (x, p) =>
    implicit val python = p
    x match {
      case x: PySet[T] => x.obj
      case x =>
        val pySet = p.newRef("set()")
        for (e <- x)
          p.exec(s"${pySet.py}.add(${e.py})")
        pySet
    }
  }


  implicit def map[K: Marshaller, V: Marshaller] = New[Map[K, V]] { (x, p) =>
    implicit val python = p
    x match {
      case x: PyDict[K, V] => x.obj
      case x =>
        val pyDict = p.newRef("{}")
        for ((k, v) <- x) {
          p.exec(s"${pyDict.py}[${k.py}] = ${v.py}")
        }
        pyDict
    }
  }

}

trait LowerPriorityMarshallers extends EvenLowerPriorityMarshallers {

  class Function0Runner[+O: Marshaller](o: Ref, f: () => O)(implicit p: Python) {
    def run() = {
      val r = f().toPython
      py"$o = $r".!
    }
  }

  class Function1Runner[-A: Unmarshaller, +O: Marshaller](a: Ref, o: Ref, f: A => O)(implicit p: Python) {
    def run() = {
      val r = f(a.toScala[A]).toPython
      py"$o = $r".!
    }
  }

  class Function2Runner[-A: Unmarshaller, -B: Unmarshaller, +O: Marshaller](a: Ref, b: Ref, o: Ref, f: (A, B) => O)(implicit p: Python) {
    def run() = {
      val r = f(a.toScala[A], b.toScala[B]).toPython
      py"$o = $r".!
    }
  }

  class Function3Runner[-A: Unmarshaller, -B: Unmarshaller, -C: Unmarshaller, +O: Marshaller](a: Ref, b: Ref, c: Ref, o: Ref, f: (A, B, C) => O)(implicit p: Python) {
    def run() = {
      val r = f(a.toScala[A], b.toScala[B], c.toScala[C]).toPython
      py"$o = $r".!
    }
  }

  implicit def function0[O: Marshaller] = New[() => O] { (x, p) =>
    implicit val python = p
    val o = p.newRef("None")
    val f = p.fromJvm(new Function0Runner(o, x))
    val pyFunc = py"""
        $f.run()
        return $o
      """
    p.defineFunction()(pyFunc.py).self
  }

  implicit def function1[A: Unmarshaller, O: Marshaller] = New[A => O] { (x, p) =>
    implicit val python = p
    val a = p.newRef("None") // create input placeholder
    val o = p.newRef("None") // create output placeholder
    val f = p.fromJvm(new Function1Runner(a, o, x))
    val pyFunc = py"""
        global $a
        $a = _arg1
        $f.run()
        return $o
      """ // write to input placeholder, run, and then return output placeholder
    p.defineFunction("_arg1")(pyFunc.py).self
  }

  implicit def function2[A: Unmarshaller, B: Unmarshaller, O: Marshaller] = New[(A, B) => O] { (x, p) =>
    implicit val python = p
    val a = p.newRef("None")
    val b = p.newRef("None")
    val o = p.newRef("None")
    val f = p.fromJvm(new Function2Runner(a, b, o, x))
    val pyFunc = py"""
        global $a
        global $b
        $a = _arg1
        $b = _arg2
        $f.run()
        return $o
      """
    p.defineFunction("_arg1", "_arg2")(pyFunc.py).self
  }

  implicit def function3[A: Unmarshaller, B: Unmarshaller, C: Unmarshaller, O: Marshaller] = New[(A, B, C) => O] { (x, p) =>
    implicit val python = p
    val a = p.newRef("None")
    val b = p.newRef("None")
    val c = p.newRef("None")
    val o = p.newRef("None")
    val f = p.fromJvm(new Function3Runner(a, b, c, o, x))
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
    p.defineFunction("_arg1", "_arg2", "_arg3")(pyFunc.py).self
  }
}

trait EvenLowerPriorityMarshallers {
  private[kingscross] case class New[T](f: (T, Python) => Expr) extends Marshaller[T] {
    def marshall(x: T)(implicit python: Python) = f(x, python)
  }

  implicit val anyRef = New[AnyRef] { (x, p) => p.fromJvm(x) }
}

//endregion
