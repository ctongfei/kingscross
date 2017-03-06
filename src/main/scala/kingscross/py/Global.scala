package kingscross.py

import jep._
import scala.language.dynamics

/**
 * @author Tongfei Chen
 */ // constructor must not be private: report to SI
class Global(implicit jep: Jep) extends Dynamic {

  def applyDynamic(method: String)(params: Expr*): Object =
    Object(s"$method(${params.map(_.py).mkString(", ")})")

  def selectDynamic(name: String): Object =
    Object(name)

}
