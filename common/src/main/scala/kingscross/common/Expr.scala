package kingscross.common

import kingscross._

/**
 * @author Tongfei Chen
 */
trait Expr[L <: Interpreter] { self: L#Expr =>

  val lang: L

  def expr: String

  def !! : lang.Ref = lang newRef expr

  def ! = lang exec expr

}
