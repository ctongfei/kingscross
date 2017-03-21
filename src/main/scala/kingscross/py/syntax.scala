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
     * Automatically trims excessive space in the start of the lines.
     */
    def py(args: Expr*): Expr = {
      val body = sc.s(args.map {
        case arg: Object => arg.py
        case arg => s"(${arg.py})"
      }: _*)
      val lines = body.split("\n").filterNot(_.trim == "")
      val indent = lines.map(_.indexWhere(_ != ' ')).min
      val r = lines map { _ drop indent } mkString "\n"
      new Expr(r)
    }

  }

}
