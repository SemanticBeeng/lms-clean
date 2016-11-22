package scala.lms
package common


trait Booleans extends Base {

  trait BooleanOps {
    def &&(y: => Boolean): Boolean
    def ||(y: => Boolean): Boolean
    def unary_! : Boolean
  }

  type Boolean <: BooleanOps

  implicit def booleanTyp: Rep[Boolean] { type U = scala.Boolean }
  implicit def booleanLift: Lift[scala.Boolean,Boolean]

}

trait BooleansImpl extends BaseExp with Booleans {


  case class And(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class Or(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class Not(e1: Exp[scala.Boolean]) extends Def[scala.Boolean]

  case class Boolean(e: Exp[scala.Boolean]) extends BooleanOps with Expressable[scala.Boolean]{
    def &&(y: => Boolean) = Boolean(And(e, y.e))
    def ||(y: => Boolean) = Boolean(Or(e, y.e))
    def unary_! = Boolean(Not(e))
  }

  private val repE = RepE[scala.Boolean, Boolean](Boolean)
  implicit val booleanTyp: Rep[Boolean] { type U = scala.Boolean } = repE
  implicit val booleanLift: Lift[scala.Boolean,Boolean] = repE

}
