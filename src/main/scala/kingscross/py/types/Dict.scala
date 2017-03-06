package kingscross.py.types

import jep._
import kingscross.py._
import scala.collection._

/**
 * Wraps a Python [[Dict]] as a Scala [[Map]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Dict[K: Marshaller, V: Marshaller](obj: Object)(implicit jep: Jep) extends Facade(obj) with DefaultMap[K, V] {

  def get(key: K) = try {
    Some(obj.index(key).toScala[V])
  } catch {
    case e: Exception => None
  }

  override def apply(key: K) = obj.index(key).toScala[V]

  def iterator = global.pythonVersion match {
    case "2" => new Iterator[(K, V)](Object(s"${obj.py}.iteritems()"))
    case "3" => new Iterator[(K, V)](Object(s"${obj.py}.items()"))
  }
}
