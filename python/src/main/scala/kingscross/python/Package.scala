package kingscross.python

import scala.language.dynamics

/**
 * @author Tongfei Chen
 */
class Package private[kingscross](name: String)(implicit python: Python) extends Dynamic {

  import python._

  def applyDynamic(method: String)(params: Expr*) = {
    if (method == "apply")
      newRef(s"$name(${params.map(_.py).mkString(", ")})")
    else
      newRef(s"$name.$method(${params.map(_.py).mkString(", ")})")
  }
  def selectDynamic(field: String) = new Expr(s"$name.$field")
  def updateDynamic(field: String)(value: Expr) = new Expr(s"$name.$field = ${value.py}").!!
}
