package scala.lms
package common

import scala.reflect.SourceContext

trait Doubles extends Base {
  self: Booleans with IfThenElse =>

  trait DoubleOps extends Num[Double] {
    self: Double =>    

    type B = Boolean
    def +(y: Double): Double
    def -(y: Double): Double
    def *(y: Double): Double
    def /(y: Double): Double
    def %(y: Double): Double

    def min(y: Double): Double =
      __ifThenElse(self < y, self, y)    

    def max(y: Double): Double =
      __ifThenElse(self > y, y, self)
    
  }
  
  type Double <: DoubleOps

  def double: Exp[scala.Double] => Double     

  implicit def doubleRep: Rep[Double] { type Internal = scala.Double }
  implicit def doubleLift: Lift[scala.Double,Double]


}

trait DoublesExp extends BaseExp with Doubles {
  self: Booleans  with IfThenElse =>

  sealed trait DoubleDef[A] extends Def[A]
  case class DoublePlus(e1: Exp[scala.Double], e2: Exp[scala.Double])  extends DoubleDef[scala.Double]
  case class DoubleMinus(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends DoubleDef[scala.Double]
  case class DoubleTimes(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends DoubleDef[scala.Double]
  case class DoubleDiv(e1: Exp[scala.Double], e2: Exp[scala.Double])   extends DoubleDef[scala.Double]
  case class DoubleMod(e1: Exp[scala.Double], e2: Exp[scala.Double])   extends DoubleDef[scala.Double]
  case class DoubleGT(e1: Exp[scala.Double], e2: Exp[scala.Double])    extends DoubleDef[scala.Boolean]
  case class DoubleLT(e1: Exp[scala.Double], e2: Exp[scala.Double])    extends DoubleDef[scala.Boolean]      


  case class Double(e: Exp[scala.Double]) extends DoubleOps with Expressable[scala.Double] {
    def +(y: Double) = double(double_plus(e, y.e))
    def -(y: Double) = double(double_minus(e, y.e))
    def *(y: Double) = double(double_times(e, y.e))
    def /(y: Double) = double(double_div(e, y.e))
    def %(y: Double) = double(double_mod(e, y.e))
    def >(y: Double) = boolean(double_gt(e, y.e))
    def <(y: Double) = boolean(double_lt(e, y.e))
  }

  def double = Double

  private val repE = RepE[scala.Double, Double](x => double(x))
  implicit val doubleRep: Rep[Double] { type Internal = scala.Double } = repE
  implicit val doubleLift: Lift[scala.Double,Double] = repE
  
  protected def double_plus(e1: Exp[scala.Double], e2: Exp[scala.Double]):  Exp[scala.Double]
  protected def double_minus(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double]
  protected def double_times(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double]
  protected def double_div(e1: Exp[scala.Double], e2: Exp[scala.Double]):   Exp[scala.Double]
  protected def double_mod(e1: Exp[scala.Double], e2: Exp[scala.Double]):   Exp[scala.Double]
  protected def double_gt(e1: Exp[scala.Double], e2: Exp[scala.Double]):    Exp[scala.Boolean]
  protected def double_lt(e1: Exp[scala.Double], e2: Exp[scala.Double]):    Exp[scala.Boolean]
}
/*
trait Derivate extends ForwardTransformer {

//  val IR: RichExp
//  import IR._

  override def transformExp[T](s: Exp[T]): Exp[T] = s match {
    case Const(s) => Const(0)
    case _ => super.transformExp(s)
  }
  
  def transformDef[T](d: Def[T]): Exp[T] = d match {
    case Plus(a,b) => Plus(transformExp(a), transformExp(b))
    case _ => super.transformDef(d)
  }
}
 */
trait DoublesImpl extends DoublesExp {
  self: BooleansExp with IfThenElse =>

  protected def double_plus(e1: Exp[scala.Double], e2: Exp[scala.Double])  = DoublePlus(e1, e2)
  protected def double_minus(e1: Exp[scala.Double], e2: Exp[scala.Double]) = DoubleMinus(e1, e2)
  protected def double_times(e1: Exp[scala.Double], e2: Exp[scala.Double]) = DoubleTimes(e1, e2)
  protected def double_div(e1: Exp[scala.Double], e2: Exp[scala.Double])   = DoubleDiv(e1, e2)
  protected def double_mod(e1: Exp[scala.Double], e2: Exp[scala.Double])   = DoubleMod(e1, e2)
  protected def double_gt(e1: Exp[scala.Double], e2: Exp[scala.Double])    = DoubleGT(e1, e2)
  protected def double_lt(e1: Exp[scala.Double], e2: Exp[scala.Double])    = DoubleLT(e1, e2)

}



trait DoublesOptImpl extends DoublesImpl with EffectExp {
  self: BooleansExp with IfThenElse =>

  override def double_plus(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => super.double_plus(e1, e2)
  }

  override def double_minus(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(0), Const(x)) => Const(-x)
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => super.double_minus(e1, e2)      
  }
  
  override def double_times(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (l, Const(0)) => Const(0)
    case (Const(x), Const(y)) => Const(x*y)
    case _ => super.double_times(e1, e2)            
  }

  override def double_div(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (Const(x), Const(y)) if x == y => Const(1)            
    case (Const(x), Const(y)) => Const(x/y)
    case _ => super.double_div(e1, e2)            
  }

  override def double_mod(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x%y)
    case (l, Const(1)) => l
    case _ => super.double_mod(e1, e2)            
  }


  override def double_gt(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x > y)
    case _ => super.double_gt(e1, e2)            
  }

  override def double_lt(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x < y)
    case _ => super.double_lt(e1, e2)            
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    e match {
      case DoublePlus(a,b) => double_plus(f(a), f(b))
      case DoubleMinus(a,b) => double_minus(f(a), f(b))        
      case DoubleTimes(a,b) => double_times(f(a), f(b))
      case DoubleDiv(a,b) => double_div(f(a), f(b))
      case DoubleGT(a,b) => double_gt(f(a), f(b))
      case DoubleLT(a,b) => double_lt(f(a), f(b))        
      case _ => super.mirror(e,f)
    }
  }.asInstanceOf[Exp[A]]
  
  
}

trait ScalaGenDoublesExp extends ScalaGenBase {
  val IR: DoublesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DoublePlus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case DoubleMinus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case DoubleTimes(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case DoubleDiv(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case DoubleGT(a,b) =>   emitValDef(sym, "" + quote(a) + ">" + quote(b))
    case DoubleLT(a,b) =>   emitValDef(sym, "" + quote(a) + "<" + quote(b))                  
    case _ => super.emitNode(sym, rhs)
  }

}

