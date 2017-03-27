package kingscross

import scala.language.higherKinds

/**
 * Represents a foreign interpreter.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Interpreter {

  type Expr
  type Ref
  type Package

  def newId: String

  def setRaw(s: String, v: Any)
  def getRaw(s: String): Any

  object rawVariables {
    def apply(s: String) = getRaw(s)
    def update(s: String, v: Any) = setRaw(s, v)
  }

  /** Runs the given statement in this interpreter. */
  def exec(s: String): Any

  /** Builds an expression object using the given expression. */
  def newExpr(s: String): Expr

  /** Runs the given expression and saves it to a named object, and returns a reference pointing to it. */
  def newRef(s: String, n: String): Ref

  def newRef(s: String): Ref = newRef(s, newId)

  /** References an object in this interpreter given the object name. */
  def getRef(n: String): Ref

  /** Loads a package given the package name. */
  def loadPackage(s: String): Package

}
