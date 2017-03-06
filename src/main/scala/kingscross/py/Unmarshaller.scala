package kingscross.py

import jep._

/**
 * @author Tongfei Chen
 */

trait Unmarshaller[+T] {
  def unmarshall(x: Expr)(implicit jep: Jep): T
}

object Unmarshaller {

  implicit object unit extends Unmarshaller[Unit] {
    def unmarshall(x: Expr)(implicit jep: Jep) = ()
  }
  implicit object int extends Unmarshaller[Int] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toPyObject.get.asInstanceOf[Int]
  }
  implicit object float extends Unmarshaller[Float] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toPyObject.get match {
      case r: Int => r
      case r: Float => r
    }
  }
  implicit object double extends Unmarshaller[Double] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toPyObject.get match {
      case r: Int => r
      case r: Float => r
      case r: Double => r
    }
  }
  implicit object boolean extends Unmarshaller[Boolean] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toPyObject.get.asInstanceOf[Boolean]
  }
  implicit object string extends Unmarshaller[String] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toPyObject.get.toString
  }
  implicit def seq[T](implicit s: Unmarshaller[T]): Unmarshaller[Seq[T]] = new Unmarshaller[Seq[T]] {
    def unmarshall(x: Expr)(implicit jep: Jep) = new types.List[T](x.toPyObject)
  }

}
