package scala.lms
package common

import scala.reflect.SourceContext

trait Floats extends Base {
  self: Booleans with IfThenElse =>

  trait FloatOps extends Num[Float] {
    self: Float =>    

    type B = Boolean
    def +(y: Float): Float
    def -(y: Float): Float
    def *(y: Float): Float
    def /(y: Float): Float
    def %(y: Float): Float

    def min(y: Float): Float =
      __ifThenElse(self < y, self, y)    

    def max(y: Float): Float =
      __ifThenElse(self > y, y, self)
    
  }
  
  type Float <: FloatOps

  def float: Exp[scala.Float] => Float     

  implicit def floatRep: Rep[Float] { type Internal = scala.Float }
  implicit def floatLift: Lift[scala.Float,Float]


}

trait FloatsExp extends BaseExp with Floats {
  self: Booleans  with IfThenElse =>

  sealed trait FloatDef[A] extends Def[A]
  case class FloatPlus(e1: Exp[scala.Float], e2: Exp[scala.Float])  extends FloatDef[scala.Float]
  case class FloatMinus(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends FloatDef[scala.Float]
  case class FloatTimes(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends FloatDef[scala.Float]
  case class FloatDiv(e1: Exp[scala.Float], e2: Exp[scala.Float])   extends FloatDef[scala.Float]
  case class FloatMod(e1: Exp[scala.Float], e2: Exp[scala.Float])   extends FloatDef[scala.Float]
  case class FloatGT(e1: Exp[scala.Float], e2: Exp[scala.Float])    extends FloatDef[scala.Boolean]
  case class FloatLT(e1: Exp[scala.Float], e2: Exp[scala.Float])    extends FloatDef[scala.Boolean]      


  case class Float(e: Exp[scala.Float]) extends FloatOps with Expressable[scala.Float] {
    def +(y: Float) = float(float_plus(e, y.e))
    def -(y: Float) = float(float_minus(e, y.e))
    def *(y: Float) = float(float_times(e, y.e))
    def /(y: Float) = float(float_div(e, y.e))
    def %(y: Float) = float(float_mod(e, y.e))
    def >(y: Float) = boolean(float_gt(e, y.e))
    def <(y: Float) = boolean(float_lt(e, y.e))
  }

  def float = Float

  private val repE = RepE[scala.Float, Float](x => float(x))
  implicit val floatRep: Rep[Float] { type Internal = scala.Float } = repE
  implicit val floatLift: Lift[scala.Float,Float] = repE
  
  protected def float_plus(e1: Exp[scala.Float], e2: Exp[scala.Float]):  Exp[scala.Float]
  protected def float_minus(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float]
  protected def float_times(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float]
  protected def float_div(e1: Exp[scala.Float], e2: Exp[scala.Float]):   Exp[scala.Float]
  protected def float_mod(e1: Exp[scala.Float], e2: Exp[scala.Float]):   Exp[scala.Float]
  protected def float_gt(e1: Exp[scala.Float], e2: Exp[scala.Float]):    Exp[scala.Boolean]
  protected def float_lt(e1: Exp[scala.Float], e2: Exp[scala.Float]):    Exp[scala.Boolean]
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
trait FloatsImpl extends FloatsExp {
  self: BooleansExp with IfThenElse =>

  protected def float_plus(e1: Exp[scala.Float], e2: Exp[scala.Float])  = FloatPlus(e1, e2)
  protected def float_minus(e1: Exp[scala.Float], e2: Exp[scala.Float]) = FloatMinus(e1, e2)
  protected def float_times(e1: Exp[scala.Float], e2: Exp[scala.Float]) = FloatTimes(e1, e2)
  protected def float_div(e1: Exp[scala.Float], e2: Exp[scala.Float])   = FloatDiv(e1, e2)
  protected def float_mod(e1: Exp[scala.Float], e2: Exp[scala.Float])   = FloatMod(e1, e2)
  protected def float_gt(e1: Exp[scala.Float], e2: Exp[scala.Float])    = FloatGT(e1, e2)
  protected def float_lt(e1: Exp[scala.Float], e2: Exp[scala.Float])    = FloatLT(e1, e2)

}



trait FloatsOptImpl extends FloatsImpl with EffectExp {
  self: BooleansExp with IfThenElse =>

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
  
  override def float_times(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (l, Const(0)) => Const(0)
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
    case FloatPlus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case FloatMinus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case FloatTimes(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case FloatDiv(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case FloatGT(a,b) =>   emitValDef(sym, "" + quote(a) + ">" + quote(b))
    case FloatLT(a,b) =>   emitValDef(sym, "" + quote(a) + "<" + quote(b))                  
    case _ => super.emitNode(sym, rhs)
  }

}


