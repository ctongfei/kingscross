package kingscross.py.types

import jep._
import kingscross.py._
import scala.collection._

/**
 * Wraps a Python [[List]] as a Scala [[Seq]].
 * @author Tongfei Chen
 */
class List[T: Unmarshaller](obj: Object)(implicit jep: Jep) extends Facade(obj) with Seq[T] {

  def length = global.len(obj).get.asInstanceOf[Int]

  def apply(idx: Int) = obj.index(Expr(idx.toString)).toScala[T]

  def iterator = new Iterator[T] {
    private[this] val pyIter = Object(s"iter(${obj.py})")
    private[this] var elem: T = _
    def hasNext = {
      if (elem != null) true
      else {
        try {
          elem = Expr(s"next(${pyIter.name})").toScala[T]
          elem != null
        }
        catch {
          case _: Exception => false //TODO: py.StopIteration
        }
      }
    }
    def next() = {
      if ((elem != null) || hasNext) {
        val r = elem
        elem = _: T
        r
      } else throw new NoSuchElementException
    }
  }
}
