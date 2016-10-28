package scala.lms
package common


trait Units extends Base {

  type Unit
  implicit def unitTyp: Rep[Unit]

}

trait UnitsImpl extends BaseExp with Units {

  case class Unit(e: Exp[scala.Unit])

  implicit val unitTyp: Rep[Unit] = new Rep[Unit] { type U = scala.Unit; def from(e:Exp[U]) = Unit(e); def to(x:Unit) = x.e; def m = manifest[U]; override def toString = "Unit" }


}
