package kingscross.py

import jep._

/**
 * Wraps a Python [[Iterator]] as a Scala [[Iterator]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Iterator[T: Unmarshaller](val pyIter: Object)(implicit jep: Jep) extends scala.collection.AbstractIterator[T] {

  private[this] var elem: T = null.asInstanceOf[T]
  def hasNext = {
    if (elem != null) true
    else {
      try {
        val pyElem = Object(s"next(${pyIter.pyName})")
        elem = pyElem.toScala[T]
        elem != null
      }
      catch {
        case _: Exception => false //TODO: kingscross.py.StopIteration
      }
    }
  }
  def next() = {
    if ((elem != null) || hasNext) {
      val r = elem
      elem = null.asInstanceOf[T]
      r
    } else throw new NoSuchElementException
  }
}
