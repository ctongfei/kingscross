package kingscross.py.typed

import jep._
import kingscross.py.{Expr, typed, _}

import scala.collection._

/**
 * Wraps a Python [[List]] as a Scala [[Seq]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
class List[T: Marshaller](val obj: Object)(implicit jep: Jep) extends mutable.AbstractSeq[T] {

  override def stringPrefix = "py.list"

  def length = global.len(obj).get.asInstanceOf[Int]

  def apply(idx: Int) = obj.index(Expr(idx.toString)).toScala[T]

  def iterator = new typed.Iterator[T](Object(s"iter(${obj.py})"))

  def update(idx: Int, elem: T) = obj.indexUpdate(Expr(idx.toString))(elem)

  def +=(elem: T) = obj.append(elem)
}

object List {
  implicit def marshaller[T: Marshaller]: Marshaller[Seq[T]] = new Marshaller[Seq[T]] {
    def marshall(x: Seq[T])(implicit jep: Jep) = x match {
      case x: List[T] => x.obj
      case _ =>
        val pyList = Object("[]")
        for (e <- x)
          jep.eval(s"${pyList.name}.append(${e.py})")
        pyList
    }
    def unmarshall(x: Expr)(implicit jep: Jep) = new List[T](x.toObject)
  }
}