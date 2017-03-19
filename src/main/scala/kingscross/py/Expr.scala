package kingscross.py

import jep._
import scala.language.dynamics

/**
 * An expression in Python.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Expr private[py](val py: String)(implicit jep: Jep) extends Dynamic {

  /**
   * Converts a Python reference to a Scala object.
   */
  def toScala[T](implicit u: Unmarshaller[T]): T = u unmarshall this

  /**
   * Converts a Python expression to a Python object, hence giving it a name.
   * This operations caches the result of the expression, making it non-lazy.
   */
  def !! = Object(py)

  /**
   * Runs this expression in Python.
   */
  def !() = {
    jep eval py
  }

  def applyDynamic(method: String)(params: Expr*) =
    Object(s"($py).$method(${params.map(_.py).mkString(", ")})")

  def applyDynamicNamed(method: String)(params: (String, Expr)*) =
    Object(s"($py).$method(${params.map { case (k, w) => s"$k=${w.py}"}.mkString(", ")})")

  def selectDynamic(field: String) =
    Object(s"($py).$field")

  def updateDynamic(field: String)(value: Expr) =
    Object(s"($py).$field = ${value.py}")

  def __getitem__(key: Expr) =
    Object(s"($py)[${key.py}]")

  def __getitem__(idx: Int) =
    Object(s"($py)[$idx]")

  def __setitem__(key: Expr)(value: Expr) =
    Expr(s"($py)[${key.py}] = ${value.py}").!()

  def __setitem__(idx: Int)(value: Expr) =
    Expr(s"($py)[$idx] = ${value.py}").!()

  def unary_+ = Expr(s"+($py)")

  def unary_- = Expr(s"-($py)")

  def +(that: Expr) = Object(s"($py) + (${that.py})")
  def -(that: Expr) = Object(s"($py) - (${that.py})")
  def *(that: Expr) = Object(s"($py) * (${that.py})")
  def /(that: Expr) = Object(s"($py) / (${that.py})")

}

object Expr {

  def apply(pysrc: String)(implicit jep: Jep) = new Expr(pysrc)

}