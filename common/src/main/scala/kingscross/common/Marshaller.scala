package kingscross.common

import kingscross._

/**
 * @author Tongfei Chen
 */
trait Marshaller[L <: Interpreter, T] { self: L#Marshaller[T] =>

  val lang: L

  def marshall(x: T): lang.Expr

}
