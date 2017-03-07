package kingscross.py.typed.numpy

import jep._
import kingscross.py._

/**
 * @author Tongfei Chen
 */
class NdArrayMarshaller[T, A](implicit nat: NdArrayType[T, A], dtype: DType[A]) extends Marshaller[T] {

  def marshall(x: T)(implicit jep: Jep) = {
    val a = nat.flatten(x)
    val jepArray = new NDArray[Array[A]](a, nat.shape(x): _*)
    Object.createNdArray(jepArray)
  }

}


class NdArrayUnmarshaller[T, A](implicit nat: NdArrayType[T, A], dtype: DType[A]) extends Unmarshaller[T] {
  def unmarshall(x: Expr)(implicit jep: Jep) = {
    val nd = jep.getValue(x.toObject.name).asInstanceOf[NDArray[Array[A]]]
    nat.unflatten(nd.getData, nd.getDimensions: _*)
  }
}
