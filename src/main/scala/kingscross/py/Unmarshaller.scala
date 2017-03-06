package kingscross.py

import jep._

import scala.annotation._

/**
 * @author Tongfei Chen
 */
@implicitNotFound("Cannot unmarshall a Python object to Scala type ${T}.")
trait Unmarshaller[+T] {

  /**
   * Converts a Python expression to a Scala object.
   */
  def unmarshall(x: Expr)(implicit jep: Jep): T

}

object Unmarshaller {

  implicit object int extends Unmarshaller[Int] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get.asInstanceOf[Int]
  }

  implicit object long extends Unmarshaller[Long] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get match {
      case r: Int => r
      case r: Long => r
    }
  }

  implicit object double extends Unmarshaller[Double] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get match {
      case r: Int => r
      case r: Float => r
      case r: Double => r
    }
  }

  implicit object float extends Unmarshaller[Float] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get match {
      case r: Int => r
      case r: Float => r
    }
  }

  implicit object boolean extends Unmarshaller[Boolean] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get.asInstanceOf[Boolean]
  }

  implicit object string extends Unmarshaller[String] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get.toString
  }

  implicit def tuple2[A: Unmarshaller, B: Unmarshaller]: Unmarshaller[(A, B)] =
    new Unmarshaller[(A, B)] {
      def unmarshall(x: Expr)(implicit jep: Jep) = (x.index(0).toScala[A], x.index(1).toScala[B])
    }

  implicit def tuple3[A: Unmarshaller, B: Unmarshaller, C: Unmarshaller]: Unmarshaller[(A, B, C)] =
    new Unmarshaller[(A, B, C)] {
      def unmarshall(x: Expr)(implicit jep: Jep) = (x.index(0).toScala[A], x.index(1).toScala[B], x.index(2).toScala[C])
    }

}