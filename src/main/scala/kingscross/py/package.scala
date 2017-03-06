package kingscross

import jep._

/**
 * @author Tongfei Chen
 */
package object py {

  def global(implicit jep: Jep) = new Global()

  implicit class ToPython[T](val x: T)(implicit m: Marshaller[T]) {
    def toPython(implicit jep: Jep) = m marshall x
  }

}
