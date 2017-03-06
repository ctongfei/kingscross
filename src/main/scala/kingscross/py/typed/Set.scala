package kingscross.py.typed

import jep._
import kingscross.py._

/**
 * Wraps a Python [[Set]] as a Scala [[Set]].
 * @author Tongfei Chen
 */
class Set[T: Marshaller](val obj: Object)(implicit jep: Jep) extends scala.collection.mutable.AbstractSet[T] {

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
  implicit def marshaller[T: Marshaller]: Marshaller[scala.collection.Set[T]] = new Marshaller[scala.collection.Set[T]] {
    def marshall(x: scala.collection.Set[T])(implicit jep: Jep) = x match {
      case x: Set[T] => x.obj
      case _ =>
        val pySet = Object("set()")
        for (e <- x)
          jep.eval(s"${pySet.name}.add(${e.py})")
        pySet
    }
    def unmarshall(x: Expr)(implicit jep: Jep) = new Set[T](x.toObject)
  }
}
