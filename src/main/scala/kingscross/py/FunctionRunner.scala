package kingscross.py

import jep._
import kingscross.py.syntax._

/**
 * @author Tongfei Chen
 */
private[py] class Function0Runner[+O: Marshaller](o: Object, f: () => O)(implicit jep: Jep) {
  def run() = {
    val b = f().toPython
    py"$o = $b".!()
  }
}

private[py] class Function1Runner[-I: Unmarshaller, +O: Marshaller]
  (i: Object, o: Object, f: I => O)(implicit jep: Jep)
{
  def run() = {
    val a = i.toScala[I]
    val b = f(a).toPython
    py"$o = $b".!()
  }
}

private[py] class Function2Runner[-I1: Unmarshaller, -I2: Unmarshaller, +O: Marshaller]
  (i1: Object, i2: Object, o: Object, f: (I1, I2) => O)(implicit jep: Jep)
{
  def run() = {
    val a1 = i1.toScala[I1]
    val a2 = i2.toScala[I2]
    val b = f(a1, a2).toPython
    py"$o = $b".!()
  }
}

