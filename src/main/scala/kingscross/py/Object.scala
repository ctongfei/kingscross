package kingscross.py

import jep._
import scala.language.dynamics

/**
 * Represents a python object.
 * @author Tongfei Chen
 */
class Object private(py: String)(implicit jep: Jep) extends Expr(py) with Dynamic {

  /**
   * @return This Python object in JVM
   */
  def _get: Any = jep.getValue(py)

  override def !! = this

  override def toString = jep.getValue(s"str($py)").asInstanceOf[String]

  override def finalize() = {
    jep.eval(s"del $py")
  }

}

object Object {

  @volatile private[this] var pyObjectId = 0

  def nextId = {
    val id = s"__$pyObjectId"
    pyObjectId += 1
    id
  }

  /** Evaluates a Python expression, represented as a string, to a Python object. */
  def apply(py: String)(implicit jep: Jep): Object = {
    val id = s"__$pyObjectId"
    jep.eval(s"$id = $py")
    pyObjectId += 1
    new Object(id)
  }

  /** Directly forwards a JVM object ([[java.lang.Object]]) to the Python side. */
  def fromJvm(x: AnyRef)(implicit jep: Jep): Object = {
    val id = s"__$pyObjectId"
    jep.set(id, x)
    pyObjectId += 1
    new Object(id)
  }

  private[py] def createNdArray[T](nd: NDArray[T])(implicit jep: Jep): Object = {
    val id = s"__$pyObjectId"
    jep.set(id, nd)
    pyObjectId += 1
    new Object(id)
  }

  implicit object unmarshaller extends Unmarshaller[Object] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.!!
  }

}
