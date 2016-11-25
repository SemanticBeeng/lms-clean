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

  implicit def floatTyp: Rep[Float] { type Internal = scala.Float }
  implicit def floatLift: Lift[scala.Float,Float]


}

trait FloatsExp extends BaseExp with Floats {
  self: BooleansExp  with IfThenElse =>

  case class FloatPlus(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends Def[scala.Float]
  case class FloatMinus(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends Def[scala.Float]
  case class FloatTimes(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends Def[scala.Float]
  case class FloatDiv(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends Def[scala.Float]
  case class FloatMod(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends Def[scala.Float]
  case class FloatEQ(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends Def[scala.Boolean]
  case class FloatGT(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends Def[scala.Boolean]
  case class FloatLT(e1: Exp[scala.Float], e2: Exp[scala.Float]) extends Def[scala.Boolean]      

  type Float <: FloatOps with Expressable[scala.Float]

  def float: Exp[scala.Float] => Float

  private val repE = RepE[scala.Float, Float](x => float(x))
  implicit val floatTyp: Rep[Float] { type Internal = scala.Float } = repE
  implicit val floatLift: Lift[scala.Float,Float] = repE
  

}

trait FloatsImpl extends FloatsExp {
  self: BooleansExp with IfThenElse =>

  case class Float(e: Exp[scala.Float]) extends FloatOps with Expressable[scala.Float] {
    def +(y: Float) = Float(FloatPlus(e, y.e))
    def -(y: Float) = Float(FloatMinus(e, y.e))
    def *(y: Float) = Float(FloatTimes(e, y.e))
    def /(y: Float) = Float(FloatDiv(e, y.e))
    def %(y: Float) = Float(FloatMod(e, y.e))
    def >(y: Float) = boolean(FloatGT(e, y.e))
    def <(y: Float) = boolean(FloatLT(e, y.e))
    def ===(y: Float) = boolean(FloatEQ(e, y.e))        
  }

  def float = Float

}



trait FloatsOptImpl extends FloatsExp with EffectExp {
  self: BooleansExp with IfThenElse =>

  case class Float(e: Exp[scala.Float]) extends FloatOps with Expressable[scala.Float] {
    def +(y: Float) = Float(float_plus(e, y.e))
    def -(y: Float) = Float(float_minus(e, y.e))
    def *(y: Float) = Float(float_times(e, y.e))    
    def /(y: Float) = Float(float_div(e, y.e))
    def %(y: Float) = Float(float_mod(e, y.e))
    def >(y: Float) = boolean(float_gt(e, y.e))
    def <(y: Float) = boolean(float_lt(e, y.e))
    def ===(y: Float) = boolean(float_equal(e, y.e))         
  }

  def float = Float

  def float_plus(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => FloatPlus(e1, e2)
  }

  def float_minus(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(0), Const(x)) => Const(-x)
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => FloatMinus(e1, e2)
  }
  
  def float_times(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (l, Const(0)) => Const(0)
    case (Const(x), Const(y)) => Const(x*y)
    case _ => FloatTimes(e1, e2)
  }

  def float_div(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (Const(x), Const(y)) if x == y => Const(1)            
    case (Const(x), Const(y)) => Const(x/y)
    case _ => FloatDiv(e1, e2)
  }

  def float_mod(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Float] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x%y)
    case (l, Const(1)) => l
    case _ => FloatMod(e1, e2)
  }


  def float_gt(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x > y)
    case _ => FloatGT(e1, e2)
  }

  def float_lt(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x < y)
    case _ => FloatLT(e1, e2)
  }

  def float_equal(e1: Exp[scala.Float], e2: Exp[scala.Float]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x == y)
    case _ => FloatEQ(e1, e2)
  }
 

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    e match {
      case FloatPlus(a,b) => float_plus(f(a), f(b))
      case FloatMinus(a,b) => float_minus(f(a), f(b))        
      case FloatTimes(a,b) => float_times(f(a), f(b))
      case FloatDiv(a,b) => float_div(f(a), f(b))
      case FloatEQ(a,b) => float_equal(f(a), f(b))
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
    case FloatEQ(a,b) =>   emitValDef(sym, "" + quote(a) + "==" + quote(b))
    case FloatGT(a,b) =>   emitValDef(sym, "" + quote(a) + ">" + quote(b))
    case FloatLT(a,b) =>   emitValDef(sym, "" + quote(a) + "<" + quote(b))                  
    case _ => super.emitNode(sym, rhs)
  }

}


