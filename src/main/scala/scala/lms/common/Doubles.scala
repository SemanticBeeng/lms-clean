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

  implicit def doubleTyp: Rep[Double] { type Internal = scala.Double }
  implicit def doubleLift: Lift[scala.Double,Double]


}

trait DoublesExp extends BaseExp with Doubles {
  self: BooleansExp  with IfThenElse =>

  case class DoublePlus(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends Def[scala.Double]
  case class DoubleMinus(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends Def[scala.Double]
  case class DoubleTimes(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends Def[scala.Double]
  case class DoubleDiv(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends Def[scala.Double]
  case class DoubleMod(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends Def[scala.Double]
  case class DoubleEQ(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends Def[scala.Boolean]
  case class DoubleGT(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends Def[scala.Boolean]
  case class DoubleLT(e1: Exp[scala.Double], e2: Exp[scala.Double]) extends Def[scala.Boolean]      

  type Double <: DoubleOps with Expressable[scala.Double]

  def double: Exp[scala.Double] => Double

  private val repE = RepE[scala.Double, Double](x => double(x))
  implicit val doubleTyp: Rep[Double] { type Internal = scala.Double } = repE
  implicit val doubleLift: Lift[scala.Double,Double] = repE
  

}

trait DoublesImpl extends DoublesExp {
  self: BooleansExp with IfThenElse =>

  case class Double(e: Exp[scala.Double]) extends DoubleOps with Expressable[scala.Double] {
    def +(y: Double) = Double(DoublePlus(e, y.e))
    def -(y: Double) = Double(DoubleMinus(e, y.e))
    def *(y: Double) = Double(DoubleTimes(e, y.e))
    def /(y: Double) = Double(DoubleDiv(e, y.e))
    def %(y: Double) = Double(DoubleMod(e, y.e))
    def >(y: Double) = boolean(DoubleGT(e, y.e))
    def <(y: Double) = boolean(DoubleLT(e, y.e))
    def ===(y: Double) = boolean(DoubleEQ(e, y.e))        
  }

  def double = Double

}



trait DoublesOptImpl extends DoublesExp with EffectExp {
  self: BooleansExp with IfThenElse =>

  case class Double(e: Exp[scala.Double]) extends DoubleOps with Expressable[scala.Double] {
    def +(y: Double) = Double(double_plus(e, y.e))
    def -(y: Double) = Double(double_minus(e, y.e))
    def *(y: Double) = Double(double_times(e, y.e))    
    def /(y: Double) = Double(double_div(e, y.e))
    def %(y: Double) = Double(double_mod(e, y.e))
    def >(y: Double) = boolean(double_gt(e, y.e))
    def <(y: Double) = boolean(double_lt(e, y.e))
    def ===(y: Double) = boolean(double_equal(e, y.e))         
  }

  def double = Double

  def double_plus(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => DoublePlus(e1, e2)
  }

  def double_minus(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(0), Const(x)) => Const(-x)
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => DoubleMinus(e1, e2)
  }
  
  def double_times(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (l, Const(0)) => Const(0)
    case (Const(x), Const(y)) => Const(x*y)
    case _ => DoubleTimes(e1, e2)
  }

  def double_div(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (Const(x), Const(y)) if x == y => Const(1)            
    case (Const(x), Const(y)) => Const(x/y)
    case _ => DoubleDiv(e1, e2)
  }

  def double_mod(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Double] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x%y)
    case (l, Const(1)) => l
    case _ => DoubleMod(e1, e2)
  }


  def double_gt(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x > y)
    case _ => DoubleGT(e1, e2)
  }

  def double_lt(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x < y)
    case _ => DoubleLT(e1, e2)
  }

  def double_equal(e1: Exp[scala.Double], e2: Exp[scala.Double]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x == y)
    case _ => DoubleEQ(e1, e2)
  }
 

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    e match {
      case DoublePlus(a,b) => double_plus(f(a), f(b))
      case DoubleMinus(a,b) => double_minus(f(a), f(b))        
      case DoubleTimes(a,b) => double_times(f(a), f(b))
      case DoubleDiv(a,b) => double_div(f(a), f(b))
      case DoubleEQ(a,b) => double_equal(f(a), f(b))
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
    case DoubleEQ(a,b) =>   emitValDef(sym, "" + quote(a) + "==" + quote(b))
    case DoubleGT(a,b) =>   emitValDef(sym, "" + quote(a) + ">" + quote(b))
    case DoubleLT(a,b) =>   emitValDef(sym, "" + quote(a) + "<" + quote(b))                  
    case _ => super.emitNode(sym, rhs)
  }

}


