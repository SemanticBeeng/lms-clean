package scala.lms
package common


trait BooleanOps[A] {
  def &&(y: => A): A
  def ||(y: => A): A
  def unary_! : A
}
  
trait Booleans extends Base {

  type Boolean <: BooleanOps[Boolean]

  implicit def booleanTyp: Rep[Boolean] { type Internal = scala.Boolean }
  implicit def booleanLift: Lift[scala.Boolean,Boolean]

}

trait BooleansExp extends BaseExp with Booleans {

  case class And(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class Or(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class Not(e1: Exp[scala.Boolean]) extends Def[scala.Boolean]

  type Boolean <: BooleanOps[Boolean] with Expressable[scala.Boolean]

  def boolean: Exp[scala.Boolean] => Boolean

  private val repE = RepE[scala.Boolean, Boolean](boolean)
  implicit val booleanTyp: Rep[Boolean] { type Internal = scala.Boolean } = repE
  implicit val booleanLift: Lift[scala.Boolean,Boolean] = repE

}

trait BooleansImpl extends BooleansExp {

  case class Boolean(e: Exp[scala.Boolean]) extends BooleanOps[Boolean] with Expressable[scala.Boolean]{
    def &&(y: => Boolean) = Boolean(And(e, y.e))
    def ||(y: => Boolean) = Boolean(Or(e, y.e))
    def unary_! = Boolean(Not(e))
  }

  def boolean = Boolean

}


trait BooleansOptImpl extends BooleansExp {

  case class Boolean(e: Exp[scala.Boolean]) extends BooleanOps[Boolean] with Expressable[scala.Boolean]{
    def &&(y: => Boolean) = Boolean(And(e, y.e))
    def ||(y: => Boolean) = Boolean(Or(e, y.e))
    def unary_! = Boolean(Not(e))
  }

  def boolean = Boolean



}
