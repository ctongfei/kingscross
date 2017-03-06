package kingscross.py

import jep._
import scala.language.dynamics

/**
 * An expression in Python.
 * @author Tongfei Chen
 */
class Expr private[py](val py: String)(implicit jep: Jep) {

  /**
   * Converts a Python reference to a Scala object.
   */
  def toScala[T](implicit s: Unmarshaller[T]): T = s.unmarshall(this)

  def toPyObject = Object(py)

  def run() = jep eval py

  def applyDynamic(method: String)(params: Expr*) =
    Expr(s"($py).$method(${params.map(_.py).mkString(", ")}")

  def applyDynamicNamed(method: String)(params: (String, Expr)*) =
    Expr(s"($py).$method(${params.map { case (k, w) => s"$k=${w.py}"}.mkString(", ")})")

  def selectDynamic(field: String) =
    Expr(s"($py).$field")

  def updateDynamic(field: String)(value: Expr) =
    Expr(s"($py).$field = ${value.py}")

  def index(key: Expr) =
    Expr(s"($py)[${key.py}]")

  def unary_+ = Expr(s"+($py)")

  def unary_- = Expr(s"-($py)")

  def +(that: Expr) = Expr(s"($py) + (${that.py})")
  def -(that: Expr) = Expr(s"($py) - (${that.py})")
  def *(that: Expr) = Expr(s"($py) * (${that.py})")
  def /(that: Expr) = Expr(s"($py) / (${that.py})")

}

object Expr {

  def apply(pysrc: String)(implicit jep: Jep) = new Expr(pysrc)

}