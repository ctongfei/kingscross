package kingscross.python

import scala.collection._
import kingscross.python.syntax._

/**
 * @author Tongfei Chen
 */
class PyIterator[T: Unmarshaller](val obj: Ref) extends AbstractIterator[T] {

  private[this] var elem: T = null.asInstanceOf[T]
  private implicit val python = obj.python
  import python._

  def hasNext = {
    if (elem != null) true
    else {
      try {
        val pyElem = newRef(s"next(${obj.py})")
        elem = pyElem.toScala[T]
        elem != null
      }
      catch {
        case _: Exception => false // StopIteration
      }
    }
  }

  def next() = {
    if ((elem != null) || hasNext) {
      val r = elem
      elem = null.asInstanceOf[T]
      r
    }
    else throw new NoSuchElementException
  }
}

class PyList[T: Marshaller : Unmarshaller](val obj: Ref) extends mutable.AbstractSeq[T] {
  implicit val python = obj.python
  import python._

  override def stringPrefix = "py.list"
  def length = Global.len(obj)._get.asInstanceOf[Int]
  def apply(idx: Int) = obj.__getitem__(newExpr(idx.toString)).toScala[T]
  def iterator = new PyIterator[T](newRef(s"iter(${obj.py})"))
  def update(idx: Int, elem: T) = obj.__setitem__(newExpr(idx.toString))(elem.toPython).!
  def +=(elem: T) = {
    obj.append(elem).!
    this
  }
}

class PySet[T: Marshaller : Unmarshaller](val obj: Ref) extends mutable.AbstractSet[T] {

  implicit val python = obj.python
  import python._

  override def stringPrefix = "py.set"
  def contains(elem: T) = newRef(s"(${elem.py}) in ${obj.py}").toScala[Boolean]
  def +=(elem: T) = {
    exec(s"${obj.py}.add(${elem.py})")
    this
  }
  def -=(elem: T) = {
    exec(s"${obj.py}.remove(${elem.py})")
    this
  }
  def iterator = new PyIterator[T](newRef(s"iter(${obj.py})"))
}

class PyDict[K: Marshaller : Unmarshaller, V: Marshaller : Unmarshaller](val obj: Ref) extends mutable.AbstractMap[K, V] {

  implicit val python = obj.python
  import python._

  override def stringPrefix = "py.dict"
  def get(key: K) = try {
    Some(obj.__getitem__(key).toScala[V])
  } catch {
    case e: Exception => None
  }
  override def apply(key: K) = obj.__getitem__(key).toScala[V]
  def iterator = Global.PythonVersion match {
    case "2" => new PyIterator[(K, V)](newRef(s"${obj.py}.iteritems()"))
    case "3" => new PyIterator[(K, V)](newRef(s"${obj.py}.items()"))
  }
  def +=(kv: (K, V)) = {
    obj.__setitem__(kv._1, kv._2).!
    this
  }
  def -=(key: K) = {
    obj.pop(key).!
    this
  }
}