package kingscross.py.types

import jep._
import kingscross.py._

/**
 * Wraps a Python [[Set]] as a Scala [[Set]].
 * @author Tongfei Chen
 */
class Set[T: Marshaller](obj: Object)(implicit jep: Jep) extends Facade(obj) with scala.collection.Set[T] {
  def contains(elem: T) = Object(s"(${elem.py}) in ${obj.name}").toScala[Boolean]
  def +(elem: T) = ???
  def -(elem: T) = ???
  def iterator = new Iterator[T](Object(s"iter(${obj.name})"))
}
