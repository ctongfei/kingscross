package kingscross

import jep._

/**
 * @author Tongfei Chen
 */
package object py {

  private[this] var __global: Global = null
  private[this] var __np: Module = null

  def global(implicit jep: Jep) = {
    if (__global == null) __global = new Global()
    __global
  }

  def np(implicit jep: Jep) = {
    if (__np == null) __np = load("numpy")
    __np
  }

  def load(s: String)(implicit jep: Jep) = Module(s)

  implicit def toPython[T](x: T)(implicit m: Marshaller[T], jep: Jep) = m marshall x

  implicit class ToPython[T](val x: T)(implicit jep: Jep) {
    def toPython(implicit m: Marshaller[T]) = m marshall x
  }


}
