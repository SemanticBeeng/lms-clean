package scala.lms
package common


trait BooleanOps[A] {
  def &&(y: => A): A
  def ||(y: => A): A
  def unary_! : A
}
  
trait Booleans extends Base {

  type Boolean <: BooleanOps[Boolean]

  implicit def booleanRep: Rep[Boolean] { type Internal = scala.Boolean }
  implicit def booleanLift: Lift[scala.Boolean,Boolean]

}

trait BooleansExp extends BaseExp with Booleans {

  case class BoolAnd(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class BoolOr(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class BoolNot(e1: Exp[scala.Boolean]) extends Def[scala.Boolean]

  case class Boolean(e: Exp[scala.Boolean]) extends BooleanOps[Boolean] with Expressable[scala.Boolean]{
    def &&(y: => Boolean) = boolean(bool_and(e, y.e))
    def ||(y: => Boolean) = boolean(bool_or(e, y.e))
    def unary_! = boolean(bool_not(e))
  }
  
  def boolean: Exp[scala.Boolean] => Boolean = Boolean

  def bool_and(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]): Exp[scala.Boolean]
  def bool_or(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]): Exp[scala.Boolean]
  def bool_not(e1: Exp[scala.Boolean]): Exp[scala.Boolean]

  private val repE = RepE[scala.Boolean, Boolean](boolean)
  implicit val booleanRep: Rep[Boolean] { type Internal = scala.Boolean } = repE
  implicit val booleanLift: Lift[scala.Boolean,Boolean] = repE

}

trait BooleansImpl extends BooleansExp {

  def bool_and(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]): Exp[scala.Boolean] = BoolAnd(e1, e2)
  def bool_or(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]): Exp[scala.Boolean] = BoolOr(e1, e2)
  def bool_not(e1: Exp[scala.Boolean]): Exp[scala.Boolean]  = BoolNot(e1)

}


trait BooleansOptImpl extends BooleansImpl {

  //TODO mirror
}
