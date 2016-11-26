package scala.lms
package common

import scala.reflect.SourceContext

trait Ints extends Base {
  self: Booleans with IfThenElse =>

  trait IntOps extends Num[Int] {
    self: Int =>    

    type B = Boolean
    def +(y: Int): Int
    def -(y: Int): Int
    def *(y: Int): Int
    def /(y: Int): Int
    def %(y: Int): Int

    def min(y: Int): Int =
      __ifThenElse(self < y, self, y)    

    def max(y: Int): Int =
      __ifThenElse(self > y, y, self)
    
  }
  
  type Int <: IntOps

  implicit def intTyp: Rep[Int] { type Internal = scala.Int }
  implicit def intLift: Lift[scala.Int,Int]


}

trait IntsExp extends BaseExp with Ints {
  self: BooleansExp  with IfThenElse =>

  case class IntPlus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class IntMinus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class IntTimes(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class IntDiv(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class IntMod(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class IntEQ(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Boolean]
  case class IntGT(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Boolean]
  case class IntLT(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Boolean]      

  type Int <: IntOps with Expressable[scala.Int]

  def int: Exp[scala.Int] => Int

  private val repE = RepE[scala.Int, Int](x => int(x))
  implicit val intTyp: Rep[Int] { type Internal = scala.Int } = repE
  implicit val intLift: Lift[scala.Int,Int] = repE
  

}

trait IntsImpl extends IntsExp {
  self: BooleansExp with IfThenElse =>

  case class Int(e: Exp[scala.Int]) extends IntOps with Expressable[scala.Int] {
    def +(y: Int) = Int(IntPlus(e, y.e))
    def -(y: Int) = Int(IntMinus(e, y.e))
    def *(y: Int) = Int(IntTimes(e, y.e))
    def /(y: Int) = Int(IntDiv(e, y.e))
    def %(y: Int) = Int(IntMod(e, y.e))
    def >(y: Int) = boolean(IntGT(e, y.e))
    def <(y: Int) = boolean(IntLT(e, y.e))
    def ===(y: Int) = boolean(IntEQ(e, y.e))        
  }

  def int = Int

}



trait IntsOptImpl extends IntsExp with EffectExp {
  self: BooleansExp with IfThenElse =>

  case class Int(e: Exp[scala.Int]) extends IntOps with Expressable[scala.Int] {
    def +(y: Int) = Int(int_plus(e, y.e))
    def -(y: Int) = Int(int_minus(e, y.e))
    def *(y: Int) = Int(int_times(e, y.e))    
    def /(y: Int) = Int(int_div(e, y.e))
    def %(y: Int) = Int(int_mod(e, y.e))
    def >(y: Int) = boolean(int_gt(e, y.e))
    def <(y: Int) = boolean(int_lt(e, y.e))
    def ===(y: Int) = boolean(int_equal(e, y.e))         
  }

  def int = Int

  def int_plus(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => IntPlus(e1, e2)
  }

  def int_minus(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(0), Const(x)) => Const(-x)
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => IntMinus(e1, e2)
  }
  
  def int_times(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (l, Const(0)) => Const(0)
    case (Const(x), Const(y)) => Const(x*y)
    case _ => IntTimes(e1, e2)
  }

  def int_div(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (Const(x), Const(y)) if x == y => Const(1)            
    case (Const(x), Const(y)) => Const(x/y)
    case _ => IntDiv(e1, e2)
  }

  def int_mod(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x%y)
    case (l, Const(1)) => l
    case _ => IntMod(e1, e2)
  }


  def int_gt(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x > y)
    case _ => IntGT(e1, e2)
  }

  def int_lt(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x < y)
    case _ => IntLT(e1, e2)
  }

  def int_equal(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x == y)
    case _ => IntEQ(e1, e2)
  }
 

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    e match {
      case IntPlus(a,b) => int_plus(f(a), f(b))
      case IntMinus(a,b) => int_minus(f(a), f(b))        
      case IntTimes(a,b) => int_times(f(a), f(b))
      case IntDiv(a,b) => int_div(f(a), f(b))
      case IntEQ(a,b) => int_equal(f(a), f(b))
      case IntGT(a,b) => int_gt(f(a), f(b))
      case IntLT(a,b) => int_lt(f(a), f(b))        
      case _ => super.mirror(e,f)
    }
  }.asInstanceOf[Exp[A]]
  
  
}

trait ScalaGenIntsExp extends ScalaGenBase {
  val IR: IntsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IntPlus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case IntMinus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case IntTimes(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case IntDiv(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case IntEQ(a,b) =>   emitValDef(sym, "" + quote(a) + "==" + quote(b))
    case IntGT(a,b) =>   emitValDef(sym, "" + quote(a) + ">" + quote(b))
    case IntLT(a,b) =>   emitValDef(sym, "" + quote(a) + "<" + quote(b))                  
    case _ => super.emitNode(sym, rhs)
  }

}


