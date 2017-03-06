package kingscross

import jep._

/**
 * @author Tongfei Chen
 */
package object py {

  def global(implicit jep: Jep) = new Global()

  def load(s: String)(implicit jep: Jep) = Module(s)

  implicit def toPython[T](x: T)(implicit m: Marshaller[T], jep: Jep) = m marshall x

  implicit class ToPython[T](val x: T)(implicit m: Marshaller[T], jep: Jep) {
    def toPython = m marshall x
  }

}
