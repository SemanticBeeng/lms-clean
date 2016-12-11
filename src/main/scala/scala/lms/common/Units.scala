package scala.lms
package common


trait Units extends Base {
  self: Longs =>

  type Unit
  implicit def unitTyp: Rep[Unit] { type Internal = scala.Unit }

  def currentNano(x:Any): Long
  def printlnR[A:Rep](x:A): Unit
}

trait UnitsExp extends BaseExp with Units {
  self: Longs =>

  case class CurrentNano(x:Exp[Any]) extends Def[scala.Long]
  case class Print(x:Exp[Any]) extends Def[scala.Unit]  
  
  case class Unit(e: Exp[scala.Unit]) extends Expressable[scala.Unit]

  def unit: Exp[scala.Unit] => Unit = Unit

  private val repE = RepE[scala.Unit, Unit](Unit)
  implicit val unitTyp = repE  

  def currentNano(x:Any): Long
  def printlnR[A:Rep](x:A): Unit

}

trait UnitsImpl extends UnitsExp {
  self: Longs =>


  def currentNano(x:Any) = long(CurrentNano(x))
  def printlnR[A:Rep](x:A) = {    
    implicit val repA = rep[A]
    unit(Print(repA.to(x)))
  }

}



trait ScalaGenUnits extends ScalaGenBase {
  val IR: UnitsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case CurrentNano(a) =>  emitValDef(sym, "System.nanoTime()")
    case Print(x) =>  emitValDef(sym, "println(" + quote(x) + ")")
    case _ => super.emitNode(sym, rhs)
  }

}


