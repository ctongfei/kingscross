package kingscross.python

import java.util.concurrent.atomic._

import jep._
import kingscross._
import kingscross.python.syntax._
import scala.collection._
import scala.language.dynamics

/**
 * Wraps a `Jep` Python interpreter.
 * @author Tongfei Chen
 */
class Python private[kingscross](val jep: Jep) extends Interpreter {

  private implicit val python = this

  type Expr = kingscross.python.Expr
  type Ref = kingscross.python.Ref
  type Package = kingscross.python.Package

  private[this] val pyObjectId = new AtomicInteger(0)

  class Function private[kingscross](val args: Seq[String], val name: String) {
    def self = newExpr(name)
  }

  object Global extends Dynamic {
    py"import sys".!
    val PythonVersion = py"sys.version_info[0]".!!.toScala[String]

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
    new Ref(n)(python)
  }

  override def newRef(s: String) = newRef(s, newId)

  def getRef(n: String) = new Ref(n)

  def fromJvm(x: AnyRef): Ref = {
    val id = newId
    setRaw(id, x)
    new Ref(id)(python)
  }

  def setRaw(s: String, v: Any) = jep.set(s, v)
  def getRaw(s: String): Any = jep getValue s
  def exec(s: String) = jep eval s

  def loadPackage(s: String) = {
    jep eval s"import $s"
    new Package(s)(python)
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

  //endregion

  //region Marshalling

  //region Unmarshalling


  //endregion

  //region Data structures wrapper

  //endregion
}

object Python {

  def apply() = new Python(new Jep())

  def withSharedModules(modules: String*) = new Python(new Jep(new JepConfig().addSharedModules(modules: _*)))

}
