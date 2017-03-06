package kingscross.py.types

import jep._
import kingscross.py._
import scala.collection._

/**
 * Wraps a Python [[List]] as a Scala [[Seq]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
class List[T: Marshaller](obj: Object)(implicit jep: Jep) extends Facade(obj) with mutable.Seq[T] {

  override def stringPrefix = "py.List"

  def length = global.len(obj).get.asInstanceOf[Int]

  def apply(idx: Int) = obj.index(Expr(idx.toString)).toScala[T]

  def iterator = new Iterator[T](Object(s"iter(${obj.py})"))

  def update(idx: Int, elem: T) = obj.indexUpdate(Expr(idx.toString))(elem)

  def +=(elem: T) = obj.append(elem)
}
