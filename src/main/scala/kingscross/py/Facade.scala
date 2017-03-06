package kingscross.py

import jep._
import scala.reflect._

/**
 * @author Tongfei Chen
 */
class Facade(val obj: Object)(implicit jep: Jep) extends Expr(obj.py) {

  override def toString = obj.toString

}

object Facade {

  implicit def marshaller[T <: Facade]: Marshaller[T] = new Marshaller[T] {
    def marshall(x: T)(implicit jep: Jep) = x
  }

  implicit def unmarshaller[T <: Facade](implicit ct: ClassTag[T]): Unmarshaller[T] = new Unmarshaller[T] {
    def unmarshall(x: Expr)(implicit jep: Jep) =
      ct.runtimeClass.getConstructor(classOf[Object], classOf[Jep]).newInstance(x.toPyObject, jep).asInstanceOf[T]
  }
}
