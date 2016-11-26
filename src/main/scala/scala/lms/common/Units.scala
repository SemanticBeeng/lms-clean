package scala.lms
package common


trait Units extends Base {

  type Unit
  implicit def unitTyp: Rep[Unit]

}

trait UnitsExp extends BaseExp with Units {

  type Unit <: Expressable[scala.Unit]

  def unit: Exp[scala.Unit] => Unit
}

trait UnitsImpl extends UnitsExp {

  case class Unit(e: Exp[scala.Unit]) extends Expressable[scala.Unit]

  def unit = Unit

  private val repE = RepE[scala.Unit, Unit](Unit)
  implicit val unitTyp: Rep[Unit] = repE

}
