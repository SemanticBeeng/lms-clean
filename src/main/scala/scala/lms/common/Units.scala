package scala.lms
package common


trait Units extends Base {

  type Unit
  implicit def unitTyp: Rep[Unit]

}

trait UnitsImpl extends BaseExp with Units {

  case class Unit(e: Exp[scala.Unit]) extends Expressable[scala.Unit]

  //  implicit val unitTyp: Rep[Unit] = new Rep[Unit] { type U = scala.Unit; def from(e:Exp[U]) = Unit(e); def to(x:Unit) = x.e; def m = manifest[U]; override def toString = "Unit" }
  private val repE = RepE[scala.Unit, Unit](Unit)
  implicit val unitTyp: Rep[Unit] = repE


}
