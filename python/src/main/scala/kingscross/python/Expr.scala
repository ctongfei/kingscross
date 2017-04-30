package kingscross.python

import scala.language.dynamics

class Expr private[kingscross](val py: String) extends Dynamic {

  def expr = py

  def !!(implicit l: Python): Ref = l newRef expr

  def applyDynamic(method: String)(params: Expr*) = new Expr(
    s"($py).$method(${params.map(_.py).mkString(", ")})"
  )

  def applyDynamicNamed(method: String)(params: (String, Expr)*) = new Expr(
    s"($py).$method(${params.map { case (k, w) => s"$k=${w.py}"}.mkString(", ")})"
  )

  def selectDynamic(field: String) = new Expr(
    s"($py).$field"
  )

  def updateDynamic(field: String, value: Expr) = new Expr(
    s"($py).$field = ${value.py}"
  )

}
