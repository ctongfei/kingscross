package kingscross.python

/**
 * @author Tongfei Chen
 */
object syntax {

  implicit def toPython[A](a: A)(implicit m: Marshaller[A], python: Python) = m marshall a

  implicit class ToPython[A](val a: A) {
    // use-site contravariance
    /** Converts this object to its corresponding representation in Python. */
    def toPython[B >: A](implicit m: Marshaller[B], python: Python) = m marshall a
  }

  implicit class ToScala(val o: Expr) {
    /** Converts this object to its corresponding representation in Scala. */
    def toScala[A](implicit u: Unmarshaller[A], python: Python) = u unmarshall o
  }

  implicit class PythonInterpolator(val sc: StringContext) {
    /**
     * Python string interpolator: executes Python script in Scala through Jep.
     * Automatically trims excessive space in the start of the lines.
     */
    def py(args: Expr*): Expr = {
      val body = sc.s(args.map {
        case arg: Ref => arg.py
        case arg      => s"(${arg.py})"
      }: _*)
      val lines = body.split("\n").filterNot(_.trim == "")
      val indent = lines.map(_.indexWhere(_ != ' ')).min
      val r = lines map { _ drop indent } mkString "\n"
      new Expr(r)
    }
  }

}
