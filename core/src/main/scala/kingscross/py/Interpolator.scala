package kingscross.py

import jep._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
object Interpolator {

  implicit class PyInterpolator(val sc: StringContext)(implicit jep: Jep) {

    /**
     * Python string interpolator: executes Python script in Scala through Jep.
     */
    def py(args: Expr*): Expr =
      new Expr(sc.s(args.map(arg => s"(${arg.py})"): _*))

  }
}
