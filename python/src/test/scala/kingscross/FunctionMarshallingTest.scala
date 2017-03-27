package kingscross

import jep._
import kingscross.python._
import kingscross.python.syntax._
import org.scalatest._

/**
 * @author Tongfei Chen
 */
class FunctionMarshallingTest extends FunSuite {

  implicit val py = Python()
  import py._


  test("Marshalling a Python function to Scala") {

    val f0 = py"lambda: 2".toScala[() => Int]
    assert(f0() == 2)

    val f1 = py"lambda x: x + 1".toScala[Int => Int]
    assert(f1(1) == 2)
    assert(f1(2) == 3)

    val f2 = py"lambda x, y: x * y".toScala[(Double, Double) => Double]
    assert(f2(2.0, 2.0) == 4.0)
    assert(f2(3.0, 4.0) == 12.0)
  }

  test("Marshalling a Scala function to Python") {

    val f0 = (() => 3).toPython
    assert(py"$f0()".!!.toScala[Int] == 3)

    val f1 = ((x: Int) => x + 2).toPython
    assert(py"$f1(2)".!!.toScala[Int] == 4)
    assert(py"$f1(1)".!!.toScala[Int] == 3)

    val f2 = ((x: Int, y: Int) => x + y).toPython
    assert(py"$f2(1, 2)".!!.toScala[Int] == 3)
    assert(py"$f2(5, 7)".!!.toScala[Int] == 12)

  }

}
