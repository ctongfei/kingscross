package kingscross.py.typed

import jep._
import kingscross.py._

/**
 * Wraps a Python [[Set]] as a Scala [[Set]].
 * @author Tongfei Chen
 */
class Set[T: Marshaller : Unmarshaller](val obj: Object)(implicit jep: Jep) extends scala.collection.mutable.AbstractSet[T] {

  override def stringPrefix = "py.set"

  def contains(elem: T) = Object(s"(${elem.py}) in ${obj.name}").toScala[Boolean]
  def +=(elem: T) = {
    jep eval s"${obj.name}.add(${elem.py})"
    this
  }
  def -=(elem: T) = {
    jep eval s"${obj.name}.remove(${elem.py})"
    this
  }
  def iterator = new Iterator[T](Object(s"iter(${obj.name})"))
}

object Set {

  implicit def unmarshaller[T: Marshaller : Unmarshaller] = new Unmarshaller[Set[T]] {
    def unmarshall(x: Expr)(implicit jep: Jep) = new Set[T](x.toObject)
  }

}