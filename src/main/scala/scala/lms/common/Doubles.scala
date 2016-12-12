package scala.lms
package common

import scala.reflect.SourceContext
import internal._

trait DoubleOps[A] extends Num[A] {
  self: A =>
}

trait Doubles extends Base {
  self: Booleans =>

  type Double <: DoubleOps[Double] { type B = Boolean }


  implicit def doubleRep: Rep[Double] { type Internal = scala.Double }
  implicit def doubleLift: Lift[scala.Double,Double]

}

trait DoublesExp extends BaseExp with Doubles {
  self: BooleansExp =>

  sealed trait DoubleDef[A] extends Def[A]
  case class DoublePlus(e1: Exp[scala.Double], e2: Exp[scala.Double])  extends DoubleDef[scala.Double]
  case class DoubleMinus(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends DoubleDef[scala.Double]
  case class DoubleTimes(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends DoubleDef[scala.Double]
  case class DoubleDiv(e1: Exp[scala.Double], e2: Exp[scala.Double])   extends DoubleDef[scala.Double]
  case class DoubleMod(e1: Exp[scala.Double], e2: Exp[scala.Double])   extends DoubleDef[scala.Double]
  case class DoubleGT(e1: Exp[scala.Double], e2: Exp[scala.Double])    extends DoubleDef[scala.Boolean]
  case class DoubleLT(e1: Exp[scala.Double], e2: Exp[scala.Double])    extends DoubleDef[scala.Boolean]
  case class DoubleMin(e1: Exp[scala.Double], e2: Exp[scala.Double])    extends DoubleDef[scala.Double]
  case class DoubleMax(e1: Exp[scala.Double], e2: Exp[scala.Double])    extends DoubleDef[scala.Double]          


  case class Double(e: Exp[scala.Double]) extends DoubleOps[Double] with Expressable[scala.Double] {

    type B = Boolean

    def +(y: Double) = double(double_plus(e, y.e))
    def -(y: Double) = double(double_minus(e, y.e))
    def *(y: Double) = double(double_times(e, y.e))
    def /(y: Double) = double(double_div(e, y.e))
    def %(y: Double) = double(double_mod(e, y.e))
    def >(y: Double) = boolean(double_gt(e, y.e))
    def <(y: Double) = boolean(double_lt(e, y.e))
    def min(y: Double): Double = double(double_min(e, y.e))
    def max(y: Double): Double = double(double_max(e, y.e))
    
  }

  def double: Exp[scala.Double] => Double = Double

  private val repE = RepE[scala.Double, Double](x => double(x))
  implicit val doubleRep: Rep[Double] { type Internal = scala.Double } = repE
  implicit val doubleLift: Lift[scala.Double,Double] = repE

  protected def double_max(e1: Exp[scala.Double], e2: Exp[scala.Double]):  Exp[scala.Double]
  protected def double_min(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double]  
  protected def double_plus(e1: Exp[scala.Double], e2: Exp[scala.Double]):  Exp[scala.Double]
  protected def double_minus(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double]
  protected def double_times(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double]
  protected def double_div(e1: Exp[scala.Double], e2: Exp[scala.Double]):   Exp[scala.Double]
  protected def double_mod(e1: Exp[scala.Double], e2: Exp[scala.Double]):   Exp[scala.Double]
  protected def double_gt(e1: Exp[scala.Double], e2: Exp[scala.Double]):    Exp[scala.Boolean]
  protected def double_lt(e1: Exp[scala.Double], e2: Exp[scala.Double]):    Exp[scala.Boolean]

}

//trait Derivate extends ForwardTransformer with BaseFatExp with RichExp {

/*
  override def transformExp[T](s: Exp[T]): Exp[T] = s match {
    case Const(s) => Const(0)
    case _ => super.transformExp(s)
  }
 */
  /*
  def transformDef[T](d: Def[T]): Exp[T] = d match {
    case DoublePlus(a,b) => DoublePlus(transformDef(a), transformDef(b))
    case _ => super.transformDef(d)
  }
   */
//}
 
trait DoublesImpl extends DoublesExp {
  self: BooleansExp  =>

  protected def double_min(e1: Exp[scala.Double], e2: Exp[scala.Double])  = DoubleMin(e1, e2)
  protected def double_max(e1: Exp[scala.Double], e2: Exp[scala.Double]) = DoubleMax(e1, e2)  
  protected def double_plus(e1: Exp[scala.Double], e2: Exp[scala.Double])  = DoublePlus(e1, e2)
  protected def double_minus(e1: Exp[scala.Double], e2: Exp[scala.Double]) = DoubleMinus(e1, e2)
  protected def double_times(e1: Exp[scala.Double], e2: Exp[scala.Double]) = DoubleTimes(e1, e2)
  protected def double_div(e1: Exp[scala.Double], e2: Exp[scala.Double])   = DoubleDiv(e1, e2)
  protected def double_mod(e1: Exp[scala.Double], e2: Exp[scala.Double])   = DoubleMod(e1, e2)
  protected def double_gt(e1: Exp[scala.Double], e2: Exp[scala.Double])    = DoubleGT(e1, e2)
  protected def double_lt(e1: Exp[scala.Double], e2: Exp[scala.Double])    = DoubleLT(e1, e2)

}



trait DoublesOptImpl extends DoublesImpl with EffectExp {
  self: BooleansExp =>

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

  override def double_min(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x.min(y))
    case _ => super.double_min(e1, e2)
  }

  override def double_max(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x.max(y))
    case _ => super.double_max(e1, e2)      
  }
  
  override def double_times(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (l, Const(0)) => Const(0)
    case (Const(1), r) => r
    case (l, Const(1)) => l
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
      case DoubleMax(a,b) => double_max(f(a), f(b))
      case DoubleMin(a,b) => double_min(f(a), f(b))        
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
    case DoublePlus(a,b) =>  emitValDef(sym, "" + quote(a) + " + " + quote(b))
    case DoubleMinus(a,b) => emitValDef(sym, "" + quote(a) + " - " + quote(b))
    case DoubleMin(a,b) => emitValDef(sym, "" + quote(a) +".min("+quote(b)+")")            
    case DoubleMax(a,b) => emitValDef(sym, "" + quote(a) +".max("+quote(b)+")")      
    case DoubleTimes(a,b) => emitValDef(sym, "" + quote(a) + " * " + quote(b))
    case DoubleDiv(a,b) =>   emitValDef(sym, "" + quote(a) + " / " + quote(b))
    case DoubleGT(a,b) =>   emitValDef(sym, "" + quote(a) + " > " + quote(b))
    case DoubleLT(a,b) =>   emitValDef(sym, "" + quote(a) + " < " + quote(b))                  
    case _ => super.emitNode(sym, rhs)
  }

}


