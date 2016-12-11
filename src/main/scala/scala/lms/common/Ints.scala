package scala.lms
package common

import scala.reflect.SourceContext
import internal._


trait IntOps[A] extends Num[A] {
  self: A =>
}

trait Ints extends Base {
  self: Booleans =>

  type Int <: IntOps[Int] { type B = Boolean }

  def int: Exp[scala.Int] => Int     

  implicit def intRep: Rep[Int] { type Internal = scala.Int }
  implicit def intLift: Lift[scala.Int,Int]

}

trait IntsExp extends BaseExp with Ints {
  self: Booleans =>

  sealed trait IntDef[A] extends Def[A]
  case class IntPlus(e1: Exp[scala.Int], e2: Exp[scala.Int])  extends IntDef[scala.Int]
  case class IntMinus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends IntDef[scala.Int]
  case class IntTimes(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends IntDef[scala.Int]
  case class IntDiv(e1: Exp[scala.Int], e2: Exp[scala.Int])   extends IntDef[scala.Int]
  case class IntMod(e1: Exp[scala.Int], e2: Exp[scala.Int])   extends IntDef[scala.Int]
  case class IntGT(e1: Exp[scala.Int], e2: Exp[scala.Int])    extends IntDef[scala.Boolean]
  case class IntLT(e1: Exp[scala.Int], e2: Exp[scala.Int])    extends IntDef[scala.Boolean]
  case class IntMin(e1: Exp[scala.Int], e2: Exp[scala.Int])    extends IntDef[scala.Int]
  case class IntMax(e1: Exp[scala.Int], e2: Exp[scala.Int])    extends IntDef[scala.Int]          


  case class Int(e: Exp[scala.Int]) extends IntOps[Int] with Expressable[scala.Int] {

    type B = Boolean

    def +(y: Int) = int(int_plus(e, y.e))
    def -(y: Int) = int(int_minus(e, y.e))
    def *(y: Int) = int(int_times(e, y.e))
    def /(y: Int) = int(int_div(e, y.e))
    def %(y: Int) = int(int_mod(e, y.e))
    def >(y: Int) = boolean(int_gt(e, y.e))
    def <(y: Int) = boolean(int_lt(e, y.e))
    def min(y: Int): Int = int(int_min(e, y.e))
    def max(y: Int): Int = int(int_max(e, y.e))
    
  }

  def int = Int

  private val repE = RepE[scala.Int, Int](x => int(x))
  implicit val intRep: Rep[Int] { type Internal = scala.Int } = repE
  implicit val intLift: Lift[scala.Int,Int] = repE

  protected def int_max(e1: Exp[scala.Int], e2: Exp[scala.Int]):  Exp[scala.Int]
  protected def int_min(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int]  
  protected def int_plus(e1: Exp[scala.Int], e2: Exp[scala.Int]):  Exp[scala.Int]
  protected def int_minus(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int]
  protected def int_times(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int]
  protected def int_div(e1: Exp[scala.Int], e2: Exp[scala.Int]):   Exp[scala.Int]
  protected def int_mod(e1: Exp[scala.Int], e2: Exp[scala.Int]):   Exp[scala.Int]
  protected def int_gt(e1: Exp[scala.Int], e2: Exp[scala.Int]):    Exp[scala.Boolean]
  protected def int_lt(e1: Exp[scala.Int], e2: Exp[scala.Int]):    Exp[scala.Boolean]

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
    case IntPlus(a,b) => IntPlus(transformDef(a), transformDef(b))
    case _ => super.transformDef(d)
  }
   */
//}
 
trait IntsImpl extends IntsExp {
  self: Booleans with IfThenElse =>

  protected def int_min(e1: Exp[scala.Int], e2: Exp[scala.Int])  = IntMin(e1, e2)
  protected def int_max(e1: Exp[scala.Int], e2: Exp[scala.Int]) = IntMax(e1, e2)  
  protected def int_plus(e1: Exp[scala.Int], e2: Exp[scala.Int])  = IntPlus(e1, e2)
  protected def int_minus(e1: Exp[scala.Int], e2: Exp[scala.Int]) = IntMinus(e1, e2)
  protected def int_times(e1: Exp[scala.Int], e2: Exp[scala.Int]) = IntTimes(e1, e2)
  protected def int_div(e1: Exp[scala.Int], e2: Exp[scala.Int])   = IntDiv(e1, e2)
  protected def int_mod(e1: Exp[scala.Int], e2: Exp[scala.Int])   = IntMod(e1, e2)
  protected def int_gt(e1: Exp[scala.Int], e2: Exp[scala.Int])    = IntGT(e1, e2)
  protected def int_lt(e1: Exp[scala.Int], e2: Exp[scala.Int])    = IntLT(e1, e2)

}



trait IntsOptImpl extends IntsImpl with EffectExp {
  self: Booleans with IfThenElse =>

  override def int_plus(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => super.int_plus(e1, e2)
  }

  override def int_minus(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(0), Const(x)) => Const(-x)
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => super.int_minus(e1, e2)      
  }

  override def int_min(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x.min(y))
    case _ => super.int_min(e1, e2)
  }

  override def int_max(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x.max(y))
    case _ => super.int_max(e1, e2)      
  }
  
  override def int_times(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (l, Const(0)) => Const(0)
    case (Const(x), Const(y)) => Const(x*y)
    case _ => super.int_times(e1, e2)            
  }

  override def int_div(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (Const(x), Const(y)) if x == y => Const(1)            
    case (Const(x), Const(y)) => Const(x/y)
    case _ => super.int_div(e1, e2)            
  }

  override def int_mod(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Int] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x%y)
    case (l, Const(1)) => l
    case _ => super.int_mod(e1, e2)            
  }


  override def int_gt(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x > y)
    case _ => super.int_gt(e1, e2)            
  }

  override def int_lt(e1: Exp[scala.Int], e2: Exp[scala.Int]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x < y)
    case _ => super.int_lt(e1, e2)            
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    e match {
      case IntMax(a,b) => int_max(f(a), f(b))
      case IntMin(a,b) => int_min(f(a), f(b))        
      case IntPlus(a,b) => int_plus(f(a), f(b))
      case IntMinus(a,b) => int_minus(f(a), f(b))        
      case IntTimes(a,b) => int_times(f(a), f(b))
      case IntDiv(a,b) => int_div(f(a), f(b))
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
    case IntPlus(a,b) =>  emitValDef(sym, "" + quote(a) + " + " + quote(b))
    case IntMinus(a,b) => emitValDef(sym, "" + quote(a) + " - " + quote(b))
    case IntMin(a,b) => emitValDef(sym, "" + quote(a) +".min("+quote(b)+")")            
    case IntMax(a,b) => emitValDef(sym, "" + quote(a) +".max("+quote(b)+")")      
    case IntTimes(a,b) => emitValDef(sym, "" + quote(a) + " * " + quote(b))
    case IntDiv(a,b) =>   emitValDef(sym, "" + quote(a) + " / " + quote(b))
    case IntGT(a,b) =>   emitValDef(sym, "" + quote(a) + " > " + quote(b))
    case IntLT(a,b) =>   emitValDef(sym, "" + quote(a) + " < " + quote(b))                  
    case _ => super.emitNode(sym, rhs)
  }

}


