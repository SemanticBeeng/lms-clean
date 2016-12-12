package scala.lms
package common

import scala.reflect.SourceContext
import internal._


trait FloatOps[A] extends Num[A] {
  self: A =>
}

trait Floats extends Base {
  self: Booleans =>

  type Float <: FloatOps[Float] { type B = Boolean }



  implicit def floatRep: Rep[Float] { type Internal = scala.Float }
  implicit def floatLift: Lift[scala.Float,Float]

}

trait FloatsExp extends BaseExp with Floats {
  self: BooleansExp =>

  sealed trait FloatDef[A] extends Def[A]
  case class FloatPlus(e1: Exp[scala.Float], e2: Exp[scala.Float])  extends FloatDef[scala.Float]
  case class FloatMinus(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends FloatDef[scala.Float]
  case class FloatTimes(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends FloatDef[scala.Float]
  case class FloatDiv(e1: Exp[scala.Float], e2: Exp[scala.Float])   extends FloatDef[scala.Float]
  case class FloatMod(e1: Exp[scala.Float], e2: Exp[scala.Float])   extends FloatDef[scala.Float]
  case class FloatGT(e1: Exp[scala.Float], e2: Exp[scala.Float])    extends FloatDef[scala.Boolean]
  case class FloatLT(e1: Exp[scala.Float], e2: Exp[scala.Float])    extends FloatDef[scala.Boolean]
  case class FloatMin(e1: Exp[scala.Float], e2: Exp[scala.Float])    extends FloatDef[scala.Float]
  case class FloatMax(e1: Exp[scala.Float], e2: Exp[scala.Float])    extends FloatDef[scala.Float]          


  case class Float(e: Exp[scala.Float]) extends FloatOps[Float] with Expressable[scala.Float] {

    type B = Boolean

    def +(y: Float) = float(float_plus(e, y.e))
    def -(y: Float) = float(float_minus(e, y.e))
    def *(y: Float) = float(float_times(e, y.e))
    def /(y: Float) = float(float_div(e, y.e))
    def %(y: Float) = float(float_mod(e, y.e))
    def >(y: Float) = boolean(float_gt(e, y.e))
    def <(y: Float) = boolean(float_lt(e, y.e))
    def min(y: Float): Float = float(float_min(e, y.e))
    def max(y: Float): Float = float(float_max(e, y.e))
    
  }

  def float: Exp[scala.Float] => Float = Float

  private val repE = RepE[scala.Float, Float](x => float(x))
  implicit val floatRep: Rep[Float] { type Internal = scala.Float } = repE
  implicit val floatLift: Lift[scala.Float,Float] = repE

  protected def float_max(e1: Exp[scala.Float], e2: Exp[scala.Float]):  Exp[scala.Float]
  protected def float_min(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float]  
  protected def float_plus(e1: Exp[scala.Float], e2: Exp[scala.Float]):  Exp[scala.Float]
  protected def float_minus(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float]
  protected def float_times(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float]
  protected def float_div(e1: Exp[scala.Float], e2: Exp[scala.Float]):   Exp[scala.Float]
  protected def float_mod(e1: Exp[scala.Float], e2: Exp[scala.Float]):   Exp[scala.Float]
  protected def float_gt(e1: Exp[scala.Float], e2: Exp[scala.Float]):    Exp[scala.Boolean]
  protected def float_lt(e1: Exp[scala.Float], e2: Exp[scala.Float]):    Exp[scala.Boolean]

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
    case FloatPlus(a,b) => FloatPlus(transformDef(a), transformDef(b))
    case _ => super.transformDef(d)
  }
   */
//}
 
trait FloatsImpl extends FloatsExp {
  self: BooleansExp  =>

  protected def float_min(e1: Exp[scala.Float], e2: Exp[scala.Float])  = FloatMin(e1, e2)
  protected def float_max(e1: Exp[scala.Float], e2: Exp[scala.Float]) = FloatMax(e1, e2)  
  protected def float_plus(e1: Exp[scala.Float], e2: Exp[scala.Float])  = FloatPlus(e1, e2)
  protected def float_minus(e1: Exp[scala.Float], e2: Exp[scala.Float]) = FloatMinus(e1, e2)
  protected def float_times(e1: Exp[scala.Float], e2: Exp[scala.Float]) = FloatTimes(e1, e2)
  protected def float_div(e1: Exp[scala.Float], e2: Exp[scala.Float])   = FloatDiv(e1, e2)
  protected def float_mod(e1: Exp[scala.Float], e2: Exp[scala.Float])   = FloatMod(e1, e2)
  protected def float_gt(e1: Exp[scala.Float], e2: Exp[scala.Float])    = FloatGT(e1, e2)
  protected def float_lt(e1: Exp[scala.Float], e2: Exp[scala.Float])    = FloatLT(e1, e2)

}



trait FloatsOptImpl extends FloatsImpl with EffectExp {
  self: BooleansExp =>

  override def float_plus(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => super.float_plus(e1, e2)
  }

  override def float_minus(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(0), Const(x)) => Const(-x)
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => super.float_minus(e1, e2)      
  }

  override def float_min(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x.min(y))
    case _ => super.float_min(e1, e2)
  }

  override def float_max(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x.max(y))
    case _ => super.float_max(e1, e2)      
  }
  
  override def float_times(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (l, Const(0)) => Const(0)
    case (Const(1), r) => r
    case (l, Const(1)) => l
    case (Const(x), Const(y)) => Const(x*y)
    case _ => super.float_times(e1, e2)            
  }

  override def float_div(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (Const(x), Const(y)) if x == y => Const(1)            
    case (Const(x), Const(y)) => Const(x/y)
    case _ => super.float_div(e1, e2)            
  }

  override def float_mod(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x%y)
    case (l, Const(1)) => l
    case _ => super.float_mod(e1, e2)            
  }


  override def float_gt(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x > y)
    case _ => super.float_gt(e1, e2)            
  }

  override def float_lt(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x < y)
    case _ => super.float_lt(e1, e2)            
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    e match {
      case FloatMax(a,b) => float_max(f(a), f(b))
      case FloatMin(a,b) => float_min(f(a), f(b))        
      case FloatPlus(a,b) => float_plus(f(a), f(b))
      case FloatMinus(a,b) => float_minus(f(a), f(b))        
      case FloatTimes(a,b) => float_times(f(a), f(b))
      case FloatDiv(a,b) => float_div(f(a), f(b))
      case FloatGT(a,b) => float_gt(f(a), f(b))
      case FloatLT(a,b) => float_lt(f(a), f(b))        
      case _ => super.mirror(e,f)
    }
  }.asInstanceOf[Exp[A]]
  
  
}

trait ScalaGenFloatsExp extends ScalaGenBase {
  val IR: FloatsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FloatPlus(a,b) =>  emitValDef(sym, "" + quote(a) + " + " + quote(b))
    case FloatMinus(a,b) => emitValDef(sym, "" + quote(a) + " - " + quote(b))
    case FloatMin(a,b) => emitValDef(sym, "" + quote(a) +".min("+quote(b)+")")            
    case FloatMax(a,b) => emitValDef(sym, "" + quote(a) +".max("+quote(b)+")")      
    case FloatTimes(a,b) => emitValDef(sym, "" + quote(a) + " * " + quote(b))
    case FloatDiv(a,b) =>   emitValDef(sym, "" + quote(a) + " / " + quote(b))
    case FloatGT(a,b) =>   emitValDef(sym, "" + quote(a) + " > " + quote(b))
    case FloatLT(a,b) =>   emitValDef(sym, "" + quote(a) + " < " + quote(b))                  
    case _ => super.emitNode(sym, rhs)
  }

}


