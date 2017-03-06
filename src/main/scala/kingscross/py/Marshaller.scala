package kingscross.py

import jep._
import kingscross.py.types._
import scala.collection._

/**
 * Represents a marshaller that converts data between Scala and Python.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Marshaller[T] {
  /**
   * Converts a Scala object to a Python expression.
   */
  def marshall(x: T)(implicit jep: Jep): Expr

  /**
   * Converts a Python expression to a Scala object.
   */
  def unmarshall(x: Expr)(implicit jep: Jep): T
}

object Marshaller {

  implicit object int extends Marshaller[Int] {
    def marshall(x: Int)(implicit jep: Jep) = Expr(s"$x")
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get.asInstanceOf[Int]
  }

  implicit object long extends Marshaller[Long] {
    def marshall(x: Long)(implicit jep: Jep) = Expr(s"${x}l")
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get match {
      case r: Int => r
      case r: Long => r
    }
  }

  implicit object double extends Marshaller[Double] {
    def marshall(x: Double)(implicit jep: Jep) = Expr(s"$x")
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get match {
      case r: Int => r
      case r: Float => r
      case r: Double => r
    }
  }

  implicit object float extends Marshaller[Float] {
    def marshall(x: Float)(implicit jep: Jep) = Expr(s"$x")
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get match {
      case r: Int => r
      case r: Float => r
    }
  }

  implicit object boolean extends Marshaller[Boolean] {
    def marshall(x: Boolean)(implicit jep: Jep) = Expr(if (x) "True" else "False")
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get.asInstanceOf[Boolean]
  }

  implicit object string extends Marshaller[String] {
    def marshall(x: String)(implicit jep: Jep) = Expr("\"" + x + "\"") // TODO: escape
    def unmarshall(x: Expr)(implicit jep: Jep) = x.toObject.get.toString
  }

  implicit def seq[T: Marshaller]: Marshaller[Seq[T]] = new Marshaller[Seq[T]] {
    def marshall(x: Seq[T])(implicit jep: Jep) = x match {
      case x: List[T] => x.obj
      case _ =>
        val pyList = Object("[]")
        for (e <- x)
          jep.eval(s"${pyList.name}.append(${e.py})")
        pyList
    }
    def unmarshall(x: Expr)(implicit jep: Jep) = new types.List[T](x.toObject)
  }

  implicit def map[K: Marshaller, V: Marshaller]: Marshaller[Map[K, V]] = new Marshaller[Map[K, V]] {
    def marshall(x: Map[K, V])(implicit jep: Jep) = x match {
      case x: Dict[K, V] => x.obj
      case _ =>
        val pyDict = Object("{}")
        for ((k, v) <- x)
          jep.eval(s"${pyDict.name}[${k.py}] = ${v.py}")
        pyDict
    }
    def unmarshall(x: Expr)(implicit jep: Jep) = new types.Dict[K, V](x.toObject)
  }

  implicit def set[T: Marshaller]: Marshaller[scala.collection.Set[T]] = new Marshaller[scala.collection.Set[T]] {
    def marshall(x: scala.collection.Set[T])(implicit jep: Jep) = x match {
      case x: types.Set[T] => x.obj
      case _ =>
        val pySet = Object("set()")
        for (e <- x)
          jep.eval(s"${pySet.name}.add(${e.py})")
        pySet
    }
    def unmarshall(x: Expr)(implicit jep: Jep) = new types.Set[T](x.toObject)
  }

  implicit def tuple2[A: Marshaller, B: Marshaller]: Marshaller[(A, B)] = new Marshaller[(A, B)] {
    def marshall(x: (A, B))(implicit jep: Jep) = Object(s"(${x._1.py}, ${x._2.py})")
    def unmarshall(x: Expr)(implicit jep: Jep) = (x.index(0).toScala[A], x.index(1).toScala[B])
  }

  implicit def tuple3[A: Marshaller, B: Marshaller, C: Marshaller]: Marshaller[(A, B, C)] = new Marshaller[(A, B, C)] {
    def marshall(x: (A, B, C))(implicit jep: Jep) = Object(s"(${x._1.py}, ${x._2.py}, ${x._3.py})")
    def unmarshall(x: Expr)(implicit jep: Jep) = (x.index(0).toScala[A], x.index(1).toScala[B], x.index(2).toScala[C])
  }

}
