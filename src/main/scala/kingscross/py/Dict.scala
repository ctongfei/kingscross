package kingscross.py

import jep._
import scala.collection._
import kingscross.py.syntax._

/**
 * Wraps a Python [[Dict]] as a Scala mutable [[Map]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Dict[K : Unmarshaller : Marshaller, V: Marshaller : Unmarshaller](val obj: Object)(implicit jep: Jep) extends mutable.AbstractMap[K, V] {

  override def stringPrefix = "py.dict"

  def get(key: K) = try {
    Some(obj.__getitem__(key).toScala[V])
  } catch {
    case e: Exception => None
  }

  override def apply(key: K) = obj.__getitem__(key).toScala[V]

  def iterator = global.pythonVersion match {
    case "2" => new Iterator[(K, V)](Object(s"${obj.py}.iteritems()"))
    case "3" => new Iterator[(K, V)](Object(s"${obj.py}.items()"))
  }

  def +=(kv: (K, V)) = {
    obj.__setitem__(kv._1)(kv._2)
    this
  }
  def -=(key: K) = {
    obj.pop(key)
    this
  }

}

object Dict {

  implicit def unmarshaller[K: Marshaller: Unmarshaller, V: Marshaller : Unmarshaller]: Unmarshaller[Dict[K, V]] = new Unmarshaller[Dict[K, V]] {
    def unmarshall(x: Expr)(implicit jep: Jep) = new Dict[K, V](x.!!)
  }

}
