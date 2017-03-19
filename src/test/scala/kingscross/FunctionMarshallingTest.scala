package kingscross

import jep._
import kingscross.py.syntax._
import org.scalatest._

/**
 * @author Tongfei Chen
 */
class FunctionMarshallingTest extends FunSuite {

  implicit val jep = new Jep(false)

  test("Marshalling a Python function to Scala") {

    val f = py"""lambda x: x + 1""".toScala[Int => Int]
    assert(f(1) == 2)
    assert(f(2) == 3)
  }

  test("Marshalling a Scala function to Python") {

    val g = ((x: Int) => x + 2).toPython
    assert(py"$g(2)".!!.toScala[Int] == 4)
    assert(py"$g(1)".!!.toScala[Int] == 3)

    val h = ((x: Int, y: Int) => x + y).toPython
    assert(py"$h(1, 2)".!!.toScala[Int] == 3)
    assert(py"$h(5, 7)".!!.toScala[Int] == 12)
  }

}
