package kingscross.py

import jep._

import scala.collection._

/**
 * Wraps a Python [[List]] as a Scala [[Seq]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
class List[T: Marshaller : Unmarshaller](val obj: Object)(implicit jep: Jep) extends mutable.AbstractSeq[T] {

  override def stringPrefix = "py.list"

  def length = global.len(obj).get.asInstanceOf[Int]

  def apply(idx: Int) = obj.__getitem__(Expr(idx.toString)).toScala[T]

  def iterator = new Iterator[T](Object(s"iter(${obj.py})"))

  def update(idx: Int, elem: T) = obj.__setitem__(Expr(idx.toString))(elem.toPython).!()

  def +=(elem: T) = {
    obj.append(elem).!()
    this
  }

}

object List {

  implicit def unmarshaller[T: Marshaller: Unmarshaller]: Unmarshaller[List[T]] = new Unmarshaller[List[T]] {
    def unmarshall(x: Expr)(implicit jep: Jep) = new List[T](x.toObject)
  }

}
