package kingscross.py

import jep._
import scala.language.dynamics

/**
 * An expression in Python.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Expr private[py](val py: String)(implicit jep: Jep) {

  /**
   * Converts a Python reference to a Scala object.
   */
  def toScala[T](implicit u: Unmarshaller[T]): T = u unmarshall this

  /**
   * Converts a Python expression to a Python object, hence giving it a name.
   * This operations caches the result of the expression, making it non-lazy.
   */
  def toObject = Object(py)

  /**
   * Runs this expression in Python.
   */
  def execute() = jep eval py

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

  def index(idx: Int) =
    Expr(s"($py)[$idx]")

  def indexUpdate(key: Expr)(value: Expr) =
    Expr(s"($py)[${key.py}] = ${value.py}")

  def indexUpdate(idx: Int)(value: Expr) =
    Expr(s"($py)[$idx] = ${value.py}")




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