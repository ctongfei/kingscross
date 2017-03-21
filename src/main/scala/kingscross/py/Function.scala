package kingscross.py

import jep._
import kingscross.py.syntax._

/**
 * Encapsulates a Python function.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Function private(val args: Seq[String], val name: String)(implicit jep: Jep) {

  /** Creates a Python lambda based on this function. */
  def lambda = {
    val argsList = args mkString ", "
    val expr = s"lambda $argsList: $name($argsList)"
    Expr(expr)
  }

  def self = Expr(name)

}

object Function {

  @volatile private[this] var pyMethodId = 0

  def define(args: String*)(body: String)(implicit jep: Jep) = {
    val id = s"__f_$pyMethodId"
    pyMethodId += 1
    val pyDef1 = s"def $id(${args.mkString(", ")}):"
    val pyDefs = body.split("\n").map(l => "    " + l).mkString("\n")
    val pyDef = s"$pyDef1\n$pyDefs\n"
    Expr(pyDef).!
    new Function(args, id)
  }

}
