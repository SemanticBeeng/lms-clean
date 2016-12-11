package scala.lms
package common

import scala.reflect.SourceContext

trait Longs extends Base {
  self: Booleans with IfThenElse =>

  trait LongOps extends Num[Long] {
    self: Long =>    

    type B = Boolean
    def +(y: Long): Long
    def -(y: Long): Long
    def *(y: Long): Long
    def /(y: Long): Long
    def %(y: Long): Long

    def min(y: Long): Long =
      __ifThenElse(self < y, self, y)    

    def max(y: Long): Long =
      __ifThenElse(self > y, y, self)
    
  }
  
  type Long <: LongOps

  def long: Exp[scala.Long] => Long     

  implicit def longRep: Rep[Long] { type Internal = scala.Long }
  implicit def longLift: Lift[scala.Long,Long]


}

trait LongsExp extends BaseExp with Longs {
  self: Booleans  with IfThenElse =>

  sealed trait LongDef[A] extends Def[A]
  case class LongPlus(e1: Exp[scala.Long], e2: Exp[scala.Long])  extends LongDef[scala.Long]
  case class LongMinus(e1: Exp[scala.Long], e2: Exp[scala.Long]) extends LongDef[scala.Long]
  case class LongTimes(e1: Exp[scala.Long], e2: Exp[scala.Long]) extends LongDef[scala.Long]
  case class LongDiv(e1: Exp[scala.Long], e2: Exp[scala.Long])   extends LongDef[scala.Long]
  case class LongMod(e1: Exp[scala.Long], e2: Exp[scala.Long])   extends LongDef[scala.Long]
  case class LongGT(e1: Exp[scala.Long], e2: Exp[scala.Long])    extends LongDef[scala.Boolean]
  case class LongLT(e1: Exp[scala.Long], e2: Exp[scala.Long])    extends LongDef[scala.Boolean]      


  case class Long(e: Exp[scala.Long]) extends LongOps with Expressable[scala.Long] {
    def +(y: Long) = long(long_plus(e, y.e))
    def -(y: Long) = long(long_minus(e, y.e))
    def *(y: Long) = long(long_times(e, y.e))
    def /(y: Long) = long(long_div(e, y.e))
    def %(y: Long) = long(long_mod(e, y.e))
    def >(y: Long) = boolean(long_gt(e, y.e))
    def <(y: Long) = boolean(long_lt(e, y.e))
  }

  def long = Long

  private val repE = RepE[scala.Long, Long](x => long(x))
  implicit val longRep: Rep[Long] { type Internal = scala.Long } = repE
  implicit val longLift: Lift[scala.Long,Long] = repE
  
  protected def long_plus(e1: Exp[scala.Long], e2: Exp[scala.Long]):  Exp[scala.Long]
  protected def long_minus(e1: Exp[scala.Long], e2: Exp[scala.Long]): Exp[scala.Long]
  protected def long_times(e1: Exp[scala.Long], e2: Exp[scala.Long]): Exp[scala.Long]
  protected def long_div(e1: Exp[scala.Long], e2: Exp[scala.Long]):   Exp[scala.Long]
  protected def long_mod(e1: Exp[scala.Long], e2: Exp[scala.Long]):   Exp[scala.Long]
  protected def long_gt(e1: Exp[scala.Long], e2: Exp[scala.Long]):    Exp[scala.Boolean]
  protected def long_lt(e1: Exp[scala.Long], e2: Exp[scala.Long]):    Exp[scala.Boolean]
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
trait LongsImpl extends LongsExp {
  self: BooleansExp with IfThenElse =>

  protected def long_plus(e1: Exp[scala.Long], e2: Exp[scala.Long])  = LongPlus(e1, e2)
  protected def long_minus(e1: Exp[scala.Long], e2: Exp[scala.Long]) = LongMinus(e1, e2)
  protected def long_times(e1: Exp[scala.Long], e2: Exp[scala.Long]) = LongTimes(e1, e2)
  protected def long_div(e1: Exp[scala.Long], e2: Exp[scala.Long])   = LongDiv(e1, e2)
  protected def long_mod(e1: Exp[scala.Long], e2: Exp[scala.Long])   = LongMod(e1, e2)
  protected def long_gt(e1: Exp[scala.Long], e2: Exp[scala.Long])    = LongGT(e1, e2)
  protected def long_lt(e1: Exp[scala.Long], e2: Exp[scala.Long])    = LongLT(e1, e2)

}



trait LongsOptImpl extends LongsImpl with EffectExp {
  self: BooleansExp with IfThenElse =>

  override def long_plus(e1: Exp[scala.Long], e2: Exp[scala.Long]): Exp[scala.Long] = (e1, e2) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => super.long_plus(e1, e2)
  }

  override def long_minus(e1: Exp[scala.Long], e2: Exp[scala.Long]): Exp[scala.Long] = (e1, e2) match {
    case (Const(0), Const(x)) => Const(-x)
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => super.long_minus(e1, e2)      
  }
  
  override def long_times(e1: Exp[scala.Long], e2: Exp[scala.Long]): Exp[scala.Long] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (l, Const(0)) => Const(0)
    case (Const(x), Const(y)) => Const(x*y)
    case _ => super.long_times(e1, e2)            
  }

  override def long_div(e1: Exp[scala.Long], e2: Exp[scala.Long]): Exp[scala.Long] = (e1, e2) match {
    case (Const(0), r) => Const(0)
    case (Const(x), Const(y)) if x == y => Const(1)            
    case (Const(x), Const(y)) => Const(x/y)
    case _ => super.long_div(e1, e2)            
  }

  override def long_mod(e1: Exp[scala.Long], e2: Exp[scala.Long]): Exp[scala.Long] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x%y)
    case (l, Const(1)) => l
    case _ => super.long_mod(e1, e2)            
  }


  override def long_gt(e1: Exp[scala.Long], e2: Exp[scala.Long]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x > y)
    case _ => super.long_gt(e1, e2)            
  }

  override def long_lt(e1: Exp[scala.Long], e2: Exp[scala.Long]): Exp[scala.Boolean] = (e1, e2) match {
    case (Const(x), Const(y)) => Const(x < y)
    case _ => super.long_lt(e1, e2)            
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    e match {
      case LongPlus(a,b) => long_plus(f(a), f(b))
      case LongMinus(a,b) => long_minus(f(a), f(b))        
      case LongTimes(a,b) => long_times(f(a), f(b))
      case LongDiv(a,b) => long_div(f(a), f(b))
      case LongGT(a,b) => long_gt(f(a), f(b))
      case LongLT(a,b) => long_lt(f(a), f(b))        
      case _ => super.mirror(e,f)
    }
  }.asInstanceOf[Exp[A]]
  
  
}

trait ScalaGenLongsExp extends ScalaGenBase {
  val IR: LongsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case LongPlus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case LongMinus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case LongTimes(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case LongDiv(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case LongGT(a,b) =>   emitValDef(sym, "" + quote(a) + ">" + quote(b))
    case LongLT(a,b) =>   emitValDef(sym, "" + quote(a) + "<" + quote(b))                  
    case _ => super.emitNode(sym, rhs)
  }

}


