package kingscross.py

import jep._
import scala.reflect._

/**
 * @author Tongfei Chen
 */
abstract class Facade(val obj: Object)(implicit jep: Jep) extends Expr(obj.py) {

  override def toString = obj.toString

}

object Facade {

  implicit def marshaller[T <: Facade](implicit ct: ClassTag[T]): Marshaller[T] = new Marshaller[T] {
    def marshall(x: T)(implicit jep: Jep) = x
    def unmarshall(x: Expr)(implicit jep: Jep) =
      ct.runtimeClass.getConstructor(classOf[Object], classOf[Jep]).newInstance(x.toObject, jep).asInstanceOf[T]
  }

}
