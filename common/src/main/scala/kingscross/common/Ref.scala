package kingscross.common

import kingscross._

/**
 * @author Tongfei Chen
 */
trait Ref[L <: Interpreter] extends Expr[L] { self: L#Ref =>

  override def !! = this.asInstanceOf[lang.Ref] // this cast is safe

}
