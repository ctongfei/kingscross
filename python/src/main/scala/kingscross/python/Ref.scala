package kingscross.python

import scala.language.dynamics

/**
 * @author Tongfei Chen
 */

class Ref private[kingscross](py: String)(implicit val python: Python) extends Expr(py) with Dynamic {
  override def finalize() = python exec s"del $py"
  override def toString() = python.getRaw(s"str($py)").asInstanceOf[String]
}
