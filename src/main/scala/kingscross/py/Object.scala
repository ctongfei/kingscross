package kingscross.py

import jep._
import scala.language.dynamics

/**
 * Represents a python object.
 * @author Tongfei Chen
 */
class Object private(val name: String)(implicit jep: Jep) extends Expr(name) with Dynamic {

  /**
   * @return This Python object in JVM
   */
  def get: Any = jep.getValue(py)

  override def toPyObject = this

  override def toString = jep.getValue(s"str($name)").asInstanceOf[String]


  override def finalize() = {
    jep.eval(s"del $name")
  }

}

object Object {

  @volatile private[this] var pyObjectId = 0

  def apply(py: String)(implicit jep: Jep): Object = {
    val id = s"__$pyObjectId"
    jep.eval(s"$id = $py")
    pyObjectId += 1
    new Object(id)
  }

}
