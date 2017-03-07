package kingscross.py.typed.numpy

/**
 * @author Tongfei Chen
 */
class DType[T]

object DType {

  object int8    extends DType[Byte]
  object int16   extends DType[Short]
  implicit object int32   extends DType[Int]
  object int64   extends DType[Long]
  object float32 extends DType[Float]
  implicit object float64 extends DType[Double]

}
