package kingscross.py.typed

import jep._
import kingscross.py._
import scala.collection._

/**
 * Wraps a Python [[Dict]] as a Scala [[Map]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Dict[K: Marshaller, V: Marshaller](val obj: Object)(implicit jep: Jep) extends scala.collection.mutable.AbstractMap[K, V] {

  override def stringPrefix = "py.dict"

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
  def +=(kv: (K, V)) = {
    obj.indexUpdate(kv._1)(kv._2)
    this
  }
  def -=(key: K) = {
    obj.pop(key.toPython)
    this
  }
}

object Dict {
  implicit def marshaller[K: Marshaller, V: Marshaller]: Marshaller[Map[K, V]] = new Marshaller[Map[K, V]] {
    def marshall(x: Map[K, V])(implicit jep: Jep) = x match {
      case x: Dict[K, V] => x.obj
      case _ =>
        val pyDict = Object("{}")
        for ((k, v) <- x)
          jep.eval(s"${pyDict.name}[${k.py}] = ${v.py}")
        pyDict
    }
    def unmarshall(x: Expr)(implicit jep: Jep) = new Dict[K, V](x.toObject)
  }
}
