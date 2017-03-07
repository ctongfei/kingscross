package kingscross.py

import jep._
import kingscross.py.numpy._

import scala.collection._
import scala.annotation._

/**
 * Represents a marshaller that converts data between Scala and Python.
 * @author Tongfei Chen
 * @since 0.1.0
 */
@implicitNotFound("Cannot marshall a Scala type ${T} to a Python representation.")
trait Marshaller[-T] {

  /**
   * Converts a Scala object to a Python expression.
   */
  def marshall(x: T)(implicit jep: Jep): Expr

}

object Marshaller {

  implicit object int extends Marshaller[Int] {
    def marshall(x: Int)(implicit jep: Jep) = Expr(s"$x")
  }

  implicit object long extends Marshaller[Long] {
    def marshall(x: Long)(implicit jep: Jep) = Expr(s"${x}l")
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

  implicit object charSequence extends Marshaller[CharSequence] {
    def marshall(x: CharSequence)(implicit jep: Jep) = Expr("\"" + x + "\"") // TODO: escape
  }

  implicit def product2[A: Marshaller, B: Marshaller]: Marshaller[Product2[A, B]] =
    new Marshaller[Product2[A, B]] {
      def marshall(x: Product2[A, B])(implicit jep: Jep) = Object(s"(${x._1.py}, ${x._2.py})")
    }

  implicit def product3[A: Marshaller, B: Marshaller, C: Marshaller]: Marshaller[Product3[A, B, C]] =
    new Marshaller[Product3[A, B, C]] {
      def marshall(x: Product3[A, B, C])(implicit jep: Jep) = Object(s"(${x._1.py}, ${x._2.py}, ${x._3.py})")
    }

  implicit def list[T: Marshaller]: Marshaller[Seq[T]] = new Marshaller[Seq[T]] {
    def marshall(x: Seq[T])(implicit jep: Jep) = x match {
      case x: List[T] => x.obj
      case _ =>
        val pyList = Object("[]")
        for (e <- x)
          jep.eval(s"${pyList.name}.append(${e.py})")
        pyList
    }
  }

  implicit def set[T: Marshaller]: Marshaller[scala.collection.Set[T]] = new Marshaller[scala.collection.Set[T]] {
    def marshall(x: scala.collection.Set[T])(implicit jep: Jep) = x match {
      case x: Set[T] => x.obj
      case _ =>
        val pySet = Object("set()")
        for (e <- x)
          jep.eval(s"${pySet.name}.add(${e.py})")
        pySet
    }
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
  }

  // Scala compiler cannot infer this marshaller correctly
  def ndarray[T, A : DType](implicit nat: NdArrayType[T, A]): Marshaller[T] =
    new NdArrayMarshaller[T, A]

  implicit def ndarrayInt[T](implicit nat: NdArrayType[T, Int]) = ndarray[T, Int](DType.int32, nat)
  implicit def ndarrayLong[T](implicit nat: NdArrayType[T, Long]) = ndarray[T, Long](DType.int64, nat)
  implicit def ndarrayFloat[T](implicit nat: NdArrayType[T, Float]) = ndarray[T, Float](DType.float32, nat)
  implicit def ndarrayDouble[T](implicit nat: NdArrayType[T, Double]) = ndarray[T, Double](DType.float64, nat)

}
