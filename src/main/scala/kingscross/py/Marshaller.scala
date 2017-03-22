package kingscross.py

import jep._
import kingscross.py.syntax._
import kingscross.py.numpy._

import scala.collection._
import scala.annotation._

/**
 * Represents a marshaller that converts data between Scala and Python.
 * @author Tongfei Chen
 * @since 0.1.0
 */
@implicitNotFound("Cannot marshall a Scala type ${T} to a Python representation.")
trait Marshaller[T] {
  //TODO: Should be contravariant, blocking on SI-2509

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

  implicit object char extends Marshaller[Char] {
    def marshall(x: Char)(implicit jep: Jep) = Expr(s"'$x'")
  }

  implicit object charSequence extends Marshaller[CharSequence] {
    def marshall(x: CharSequence)(implicit jep: Jep) = Object.fromJvm(x.toString)
  }

  implicit object string extends Marshaller[String] {
    def marshall(x: String)(implicit jep: Jep) = Object.fromJvm(x)
  }

  implicit object pyExpr extends Marshaller[Expr] {
    def marshall(x: Expr)(implicit jep: Jep) = x
  }

  implicit object pyObject extends Marshaller[Object] {
    def marshall(x: Object)(implicit jep: Jep) = x
  }

  implicit object anyRef extends Marshaller[AnyRef] {
    def marshall(x: AnyRef)(implicit jep: Jep) = Object.fromJvm(x)
  }

  implicit def product1[A: Marshaller]: Marshaller[Product1[A]] =
    new Marshaller[Product1[A]] {
      def marshall(x: Product1[A])(implicit jep: Jep) = Object(s"(${x._1.py}, )")
    }

  implicit def product2[A: Marshaller, B: Marshaller]: Marshaller[Product2[A, B]] =
    new Marshaller[Product2[A, B]] {
      def marshall(x: Product2[A, B])(implicit jep: Jep) = Object(s"(${x._1.py}, ${x._2.py})")
    }

  implicit def product3[A: Marshaller, B: Marshaller, C: Marshaller]: Marshaller[Product3[A, B, C]] =
    new Marshaller[Product3[A, B, C]] {
      def marshall(x: Product3[A, B, C])(implicit jep: Jep) = Object(s"(${x._1.py}, ${x._2.py}, ${x._3.py})")
    }

  implicit def tuple1[A: Marshaller]: Marshaller[Tuple1[A]] =
    new Marshaller[Tuple1[A]] {
      def marshall(x: Tuple1[A])(implicit jep: Jep) = Object(s"(${x._1.py}, )")
    }

  implicit def tuple2[A: Marshaller, B: Marshaller]: Marshaller[Tuple2[A, B]] =
    new Marshaller[Tuple2[A, B]] {
      def marshall(x: Tuple2[A, B])(implicit jep: Jep) = Object(s"(${x._1.py}, ${x._2.py})")
    }

  implicit def tuple3[A: Marshaller, B: Marshaller, C: Marshaller]: Marshaller[Tuple3[A, B, C]] =
    new Marshaller[Tuple3[A, B, C]] {
      def marshall(x: Tuple3[A, B, C])(implicit jep: Jep) = Object(s"(${x._1.py}, ${x._2.py}, ${x._3.py})")
    }

  implicit def function0[A: Marshaller]: Marshaller[() => A] =
    new Marshaller[() => A] {
      def marshall(x: () => A)(implicit jep: Jep) = {
        val o = Object("None")
        val f = Object.fromJvm(new Function0Runner(o, x))
        val pyFunc = py"""
          $f.run()
          return $o
        """
        Function.define()(pyFunc.py).self
      }
    }

  implicit def function1[A: Unmarshaller, B: Marshaller]: Marshaller[A => B] =
    new Marshaller[A => B] {
      def marshall(x: A => B)(implicit jep: Jep) = {
        val i = Object("None") // create input placeholder
        val o = Object("None") // create output placeholder
        val f = Object.fromJvm(new Function1Runner(i, o, x)) // to Python
        val pyFunc = py"""
          global $i
          $i = _arg1
          $f.run()
          return $o
        """ // write to input placeholder, run, and then return output placeholder
        Function.define("_arg1")(pyFunc.py).self
      }
    }

  implicit def function2[A: Unmarshaller, B: Unmarshaller, C: Marshaller]: Marshaller[(A, B) => C] =
    new Marshaller[(A, B) => C] {
      def marshall(x: (A, B) => C)(implicit jep: Jep) = {
        val ia = Object("None")
        val ib = Object("None")
        val o = Object("None")
        val f = Object.fromJvm(new Function2Runner(ia, ib, o, x))
        val pyFunc = py"""
          global $ia
          global $ib
          $ia = _arg1
          $ib = _arg2
          $f.run()
          return $o
        """
        Function.define("_arg1", "_arg2")(pyFunc.py).self
      }
    }

  implicit def function3[A: Unmarshaller, B: Unmarshaller, C: Unmarshaller, D: Marshaller]: Marshaller[(A, B, C) => D] =
    new Marshaller[(A, B, C) => D] {
      def marshall(x: (A, B, C) => D)(implicit jep: Jep) = {
        val ia = Object("None")
        val ib = Object("None")
        val ic = Object("None")
        val o = Object("None")
        val f = Object.fromJvm(new Function3Runner(ia, ib, ic, o, x))
        val pyFunc = py"""
          global $ia
          global $ib
          global $ic
          $ia = _arg1
          $ib = _arg2
          $ic = _arg3
          $f.run()
          return $o
        """
        Function.define("_arg1", "_arg2", "_arg3")(pyFunc.py).self
      }
    }

  implicit def list[T: Marshaller]: Marshaller[Seq[T]] = new Marshaller[Seq[T]] {
    def marshall(x: Seq[T])(implicit jep: Jep) = x match {
      case x: List[T] => x.obj
      case _ =>
        val pyList = Object("[]")
        for (e <- x)
          jep.eval(s"${pyList.pyName}.append(${e.py})")
        pyList
    }
  }

  implicit def set[T: Marshaller]: Marshaller[scala.collection.Set[T]] = new Marshaller[scala.collection.Set[T]] {
    def marshall(x: scala.collection.Set[T])(implicit jep: Jep) = x match {
      case x: Set[T] => x.obj
      case _ =>
        val pySet = Object("set()")
        for (e <- x)
          jep.eval(s"${pySet.pyName}.add(${e.py})")
        pySet
    }
  }

  implicit def map[K: Marshaller, V: Marshaller]: Marshaller[Map[K, V]] = new Marshaller[Map[K, V]] {
    def marshall(x: Map[K, V])(implicit jep: Jep) = x match {
      case x: Dict[K, V] => x.obj
      case _ =>
        val pyDict = Object("{}")
        for ((k, v) <- x)
          jep.eval(s"${pyDict.pyName}[${k.py}] = ${v.py}")
        pyDict
    }
  }

}
