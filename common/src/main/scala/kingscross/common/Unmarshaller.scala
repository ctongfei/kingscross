package kingscross.common

import kingscross._
/**
 * @author Tongfei Chen
 */
trait Unmarshaller[L <: Interpreter, T] {

  val lang: L

  def unmarshall(x: lang.Expr): T

}
