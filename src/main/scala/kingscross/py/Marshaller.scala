package kingscross.py

import jep._
import kingscross.py.types.List

import scala.collection._

/**
 * @author Tongfei Chen
 */
trait Marshaller[-T] {
  def marshall(x: T)(implicit jep: Jep): Expr
}

object Marshaller {

  implicit object int extends Marshaller[Int] {
    def marshall(x: Int)(implicit jep: Jep) = Expr(s"$x")
  }

  implicit object double extends Marshaller[Double] {
    def marshall(x: Double)(implicit jep: Jep) = Expr(s"$x")
  }

  implicit object float extends Marshaller[Float] {
    def marshall(x: Float)(implicit jep: Jep) = Expr(s"$x")
  }

  implicit object boolean extends Marshaller[Boolean] {
    def marshall(x: Boolean)(implicit jep: Jep) = Expr(if (x) "True" else "False")
  }

  implicit object string extends Marshaller[String] {
    def marshall(x: String)(implicit jep: Jep) = Expr("\"" + x + "\"") // TODO: escape
  }

  implicit def seq[T: Marshaller]: Marshaller[Seq[T]] = new Marshaller[Seq[T]] {
    def marshall(x: Seq[T])(implicit jep: Jep) = x match {
      case x: List[T] => x.obj
      case _ =>
        val pyList = Object("[]")
        for (e <- x)
          jep.eval(s"${pyList.name}.append(${e.toPython.py})")
        pyList
    }
  }

  implicit def map[K: Marshaller, V: Marshaller]: Marshaller[Map[K, V]] = new Marshaller[Map[K, V]] {
    def marshall(x: Map[K, V])(implicit jep: Jep) = ???
  }

}
