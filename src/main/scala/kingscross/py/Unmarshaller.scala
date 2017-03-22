package kingscross.py

import jep._
import kingscross.py.syntax._

import scala.annotation._

/**
 * Represents a strategy of unmarshalling a Python object to a Scala object of a specific type.
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
    def unmarshall(x: Expr)(implicit jep: Jep) = x.!!._get.asInstanceOf[Int]
  }

  implicit object long extends Unmarshaller[Long] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.!!._get match {
      case r: Int => r
      case r: Long => r
    }
  }

  implicit object double extends Unmarshaller[Double] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.!!._get match {
      case r: Int => r
      case r: Float => r
      case r: Double => r
    }
  }

  implicit object float extends Unmarshaller[Float] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.!!._get match {
      case r: Int => r
      case r: Float => r
    }
  }

  implicit object boolean extends Unmarshaller[Boolean] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.!!._get.asInstanceOf[Boolean]
  }

  implicit object string extends Unmarshaller[String] {
    def unmarshall(x: Expr)(implicit jep: Jep) = x.!!._get.toString
  }

  implicit def function0[A: Unmarshaller]: Unmarshaller[() => A] =
    new Unmarshaller[() => A] {
      def unmarshall(x: Expr)(implicit jep: Jep) = () => {
        val pyA = x.__call__()
        pyA.toScala[A]
      }
    }

  implicit def function1[A: Marshaller, B: Unmarshaller]: Unmarshaller[A => B] =
    new Unmarshaller[A => B] {
      def unmarshall(x: Expr)(implicit jep: Jep) = (a: A) => {
        val pyA = a.toPython
        val pyB = x.__call__(pyA)
        pyB.toScala[B]
      }
    }

  implicit def function2[A: Marshaller, B: Marshaller, C: Unmarshaller]: Unmarshaller[(A, B) => C] =
    new Unmarshaller[(A, B) => C] {
      def unmarshall(x: Expr)(implicit jep: Jep) = (a: A, b: B) => {
        val pyA = a.toPython
        val pyB = b.toPython
        val pyC = x.__call__(pyA, pyB)
        pyC.toScala[C]
      }
    }

  implicit def function3[A: Marshaller, B: Marshaller, C: Marshaller, D: Unmarshaller]: Unmarshaller[(A, B, C) => D] =
    new Unmarshaller[(A, B, C) => D] {
      def unmarshall(x: Expr)(implicit jep: Jep) = (a: A, b: B, c: C) => {
        val pyA = a.toPython
        val pyB = b.toPython
        val pyC = c.toPython
        val pyD = x.__call__(pyA, pyB, pyC)
        pyD.toScala[D]
      }
    }

  implicit def tuple1[A: Unmarshaller]: Unmarshaller[Tuple1[A]] =
    new Unmarshaller[Tuple1[A]] {
      def unmarshall(x: Expr)(implicit jep: Jep) = Tuple1(x.__getitem__(0).toScala[A])
    }

  implicit def tuple2[A: Unmarshaller, B: Unmarshaller]: Unmarshaller[(A, B)] =
    new Unmarshaller[(A, B)] {
      def unmarshall(x: Expr)(implicit jep: Jep) = (x.__getitem__(0).toScala[A], x.__getitem__(1).toScala[B])
    }

  implicit def tuple3[A: Unmarshaller, B: Unmarshaller, C: Unmarshaller]: Unmarshaller[(A, B, C)] =
    new Unmarshaller[(A, B, C)] {
      def unmarshall(x: Expr)(implicit jep: Jep) = (x.__getitem__(0).toScala[A], x.__getitem__(1).toScala[B], x.__getitem__(2).toScala[C])
    }

}
