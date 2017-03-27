package kingscross

import scala.language.higherKinds

/**
 * Represents a foreign interpreter.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Interpreter { self =>

  type Expr <: common.Expr[this.type]

  type Ref <: common.Ref[this.type] with Expr

  type Package <: common.Package[this.type]

  type Marshaller[T] <: common.Marshaller[this.type, T]

  type Unmarshaller[T] <: common.Unmarshaller[this.type, T]

  def newId: String

  def setRaw(s: String, v: Any)
  def getRaw(s: String): Any

  def set[A](s: String, v: A)(implicit m: Marshaller[A]) = newRef(s, m.marshall(v).expr)
  def get[A](s: String)(implicit u: Unmarshaller[A]) = u.unmarshall(getRef(s))

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
