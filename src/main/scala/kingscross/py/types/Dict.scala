package kingscross.py.types

import jep._
import kingscross.py._
import scala.collection._

/**
 * @author Tongfei Chen
 */
class Dict[K: Unmarshaller : Marshaller, V: Unmarshaller](obj: Object)(implicit jep: Jep) extends Facade(obj) with DefaultMap[K, V] {

  def get(key: K) = try {
    Some(obj.index(key.toPython).toScala[V])
  } catch {
    case e: Exception => None
  }

  override def apply(key: K) = obj.index(key.toPython).toScala[V]

  def iterator = ???
}
