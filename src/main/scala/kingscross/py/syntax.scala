package kingscross.py

import jep._

/**
 * @author Tongfei Chen
 */
object syntax {

  implicit def toPython[T](x: T)(implicit m: Marshaller[T], jep: Jep) = m marshall x

  implicit class ToPython[T](val x: T)(implicit jep: Jep) {
    def toPython(implicit m: Marshaller[T]) = m marshall x
  }

  implicit class PyInterpolator(val sc: StringContext)(implicit jep: Jep) {

    /**
     * Python string interpolator: executes Python script in Scala through Jep.
     */
    def py(args: Expr*): Expr =
      new Expr(sc.s(args.map(arg => s"(${arg.py})"): _*))

  }

}
