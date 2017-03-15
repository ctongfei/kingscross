package kingscross.py.numpy

import scala.reflect._

/**
 * @author Tongfei Chen
 */
trait NdArrayType[T, A] {
  def classTagA: ClassTag[A]
  def classTagT: ClassTag[T]
  def newArray(ns: Int*): T
  def dim: Int
  def flatten(a: T): Array[A]
  def unflatten(a: Array[A], ns: Int*): T
  def shape(a: T): Array[Int]
}

object NdArrayType {

  implicit def type1[A](implicit ct: ClassTag[A]): NdArrayType[Array[A], A] = new NdArrayType[Array[A], A] {
    def classTagA = ct
    def classTagT = ct.wrap
    def newArray(ns: Int*) = ct.newArray(ns.head)
    def dim = 1
    def flatten(a: Array[A]) = a
    def unflatten(a: Array[A], ns: Int*) = a
    def shape(a: Array[A]) = Array(a.length)
  }

  implicit def typeN[T, A](implicit t: NdArrayType[T, A]): NdArrayType[Array[T], A] = new NdArrayType[Array[T], A] {
    def classTagA = t.classTagA
    def classTagT = t.classTagT.wrap
    def newArray(ns: Int*) = Array.tabulate(ns.head)(i => t.newArray(ns.tail: _*))(t.classTagT)
    def dim = t.dim + 1
    def flatten(a: Array[T]) = a.flatMap(t.flatten).toArray(classTagA)
    def unflatten(a: Array[A], ns: Int*) = {
      val sliceSize = a.length / ns.head
      Array.tabulate(ns.head)(i => t.unflatten(a.slice(sliceSize * i, sliceSize * (i + 1)), ns.tail: _*))(t.classTagT)
    }
    def shape(a: Array[T]) = a.length +: t.shape(a.head)
  }

}
