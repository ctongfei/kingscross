package kingscross.python

import kingscross.python.syntax._
import scala.annotation._

/**
 * @author Tongfei Chen
 */
@implicitNotFound("Cannot marshall a Python object to a Scala representation of type ${T}.")
trait Unmarshaller[T] {
  def unmarshall(x: Expr)(implicit python: Python): T
}

object Unmarshaller {
  private[kingscross] case class New[T](f: (Expr, Python) => T) extends Unmarshaller[T] {
    def unmarshall(x: Expr)(implicit python: Python) = f(x, python)
  }

  implicit val int = New[Int] { (x, p) => p.getRaw(x.!!(p).py).asInstanceOf[Int] }
  implicit val long = New[Long] { (x, p) => p.getRaw(x.!!(p).py).asInstanceOf[Long] }
  implicit val double = New[Double] { (x, p) => p.getRaw(x.!!(p).py).asInstanceOf[Double] }
  implicit val float = New[Float] { (x, p) => p.getRaw(x.!!(p).py).asInstanceOf[Float] }
  implicit val boolean = New[Boolean] { (x, p) => p.getRaw(x.!!(p).py).asInstanceOf[Boolean] }
  implicit val string = New[String] { (x, p) => p.getRaw(x.!!(p).py).toString }

  implicit def function0[O: Unmarshaller] = New[() => O] {
    (x, p) => {
      implicit val python = p
      () => x.__call__().toScala[O]
    }
  }

  implicit def function1[A: Marshaller, O: Unmarshaller] = New[A => O] {
    (x, p) => {
      implicit val python = p
      a => x.__call__(a.toPython).toScala[O]
    }
  }

  implicit def function2[A: Marshaller, B: Marshaller, O: Unmarshaller] = New[(A, B) => O] {
    (x, p) => {
      implicit val python = p
      (a, b) => x.__call__(a.toPython, b.toPython).toScala[O]
    }
  }

  implicit def function3[A: Marshaller, B: Marshaller, C: Marshaller, O: Unmarshaller] = New[(A, B, C) => O] {
    (x, p) => {
      implicit val python = p
      (a, b, c) => x.__call__(a.toPython, b.toPython, c.toPython).toScala[O]
    }
  }

  implicit def tuple1[A: Unmarshaller] = New[Tuple1[A]] {
    (x, p) => {
      implicit val python = p
      Tuple1(x.__getitem__(0).toScala[A])
    }
  }

  implicit def tuple2[A: Unmarshaller, B: Unmarshaller] = New[Tuple2[A, B]] {
    (x, p) => {
      implicit val python = p
      Tuple2(x.__getitem__(0).toScala[A], x.__getitem__(1).toScala[B])
    }
  }

  implicit def tuple3[A: Unmarshaller, B: Unmarshaller, C: Unmarshaller] = New[Tuple3[A, B, C]] {
    (x, p) => {
      implicit val python = p
      Tuple3(x.__getitem__(0).toScala[A], x.__getitem__(1).toScala[B], x.__getitem__(2).toScala[C])
    }
  }

  implicit def iterator[A: Unmarshaller] = New[PyIterator[A]] {
    (x, p) => new PyIterator[A](x.!!(p))
  }

  implicit def list[A: Marshaller : Unmarshaller] = New[PyList[A]] {
    (x, p) => new PyList[A](x.!!(p))
  }

  implicit def set[A: Marshaller : Unmarshaller] = New[PySet[A]] {
    (x, p) => new PySet[A](x.!!(p))
  }

  implicit def dict[K: Marshaller : Unmarshaller, V: Marshaller : Unmarshaller] = New[PyDict[K, V]] {
    (x, p) => new PyDict[K, V](x.!!(p))
  }
}
