package scala.lms
package common


trait Booleans extends Base {

  trait BooleanOps {
    def &&(y: => Boolean): Boolean
    def ||(y: => Boolean): Boolean
    def unary_! : Boolean
  }

  type Boolean <: BooleanOps

  implicit def booleanTyp: Rep[Boolean]
  implicit def booleanLift: Lift[scala.Boolean,Boolean]

}

trait BooleansImpl extends BaseExp with Booleans {


  case class And(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class Or(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class Not(e1: Exp[scala.Boolean]) extends Def[scala.Boolean]

  case class Boolean(e: Exp[scala.Boolean]) extends BooleanOps {
    def &&(y: => Boolean) = Boolean(And(e, y.e))
    def ||(y: => Boolean) = Boolean(Or(e, y.e))
    def unary_! = Boolean(Not(e))
  }

  implicit val booleanTyp: Rep[Boolean] = new Rep[Boolean] { type U = scala.Boolean; def from(e:Exp[U]) = Boolean(e); def to(x:Boolean) = x.e; def m = manifest[U]; override def toString = "Boolean" }
  implicit val booleanLift: Lift[scala.Boolean,Boolean] = new Lift[scala.Boolean,Boolean] { def to(x:scala.Boolean) = Boolean(unit(x)) }

}
