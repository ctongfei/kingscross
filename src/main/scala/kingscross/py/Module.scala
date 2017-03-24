package kingscross.py

import jep._
import scala.language.dynamics

/**
 * Represents a Python module.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Module private(val name: String)(implicit jep: Jep) extends Dynamic {

  def applyDynamic(method: String)(params: Expr*) = {
    if (method == "apply")
      Object(s"$name(${params.map(_.py).mkString(", ")})")
    else
      Object(s"$name.$method(${params.map(_.py).mkString(", ")})")
  }

  def selectDynamic(field: String) = Expr(s"$name.$field").!!

  def updateDynamic(field: String)(value: Expr) = jep eval s"$name.$field = ${value.py}"

}

object Module {

  @volatile private[this] var pyModuleId = 0

  def apply(name: String)(implicit jep: Jep) = {
    val id = s"__m_$pyModuleId"
    pyModuleId += 1
    jep eval s"import $name as $id"
    new Module(id)
  }


}
