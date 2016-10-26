package scala.lms

import internal._
import scala.annotation.implicitNotFound


trait Base {
  // preliminaries
  @implicitNotFound("${T} is not a DSL type")
  @implicitNotFound("${A} cannot be implicitly lifted to ${B}")
  type Lift[A,B]
  type Exp[T]
  implicit def identLift[T:Rep]: Lift[T,T]
  implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U

  trait Rep[T] {
    type U
    def from(e:Exp[U]): T
    def to(x:T):Exp[U]
    def m: Manifest[U]
  }

/*  case class Rewrite[T:Rep](a:T, b:T)

  def lower[A:Rep,B:Rep,C:Rep](f: (A,B) => Rewrite[C]): Unit
 */
}

trait BaseExp extends Base with Expressions {

  trait Lift[A,B] {
    def to(x:A):B
  }

  implicit def identLift[T:Rep]: Lift[T,T] = new Lift[T,T] { def to(x:T) = x }
  implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U = e.to(x)

  def typ[T:Rep] = implicitly[Rep[T]]

}


trait DSL extends Base {
  trait IntOps {
    def +(y: Int): Int
    def -(y: Int): Int
    def *(y: Int): Int
    def /(y: Int): Int
    def %(y: Int): Int
  }
  trait BooleanOps {
    def &&(y: => Boolean): Boolean
    def ||(y: => Boolean): Boolean
    def unary_! : Boolean
  }
  type Int <: IntOps
  type Boolean <: BooleanOps

  type Unit

  implicit def intTyp: Rep[Int]
  implicit def intLift: Lift[scala.Int,Int]
  implicit def booleanTyp: Rep[Boolean]
  implicit def booleanLift: Lift[scala.Boolean,Boolean]

  trait ArrayOps[T] {
    def length: Int
    def apply(x: Int): T
    def update(x: Int, y: T): Unit
  }

  type Array[T] <: ArrayOps[T]
  def NewArray[T:Rep](x: Int): Array[T]
  implicit def arrayTyp[T:Rep]: Rep[Array[T]]

//  def __ifThenElse[C,A,B](c:Boolean, a: =>A, b: =>B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Rep[C]): C

  // tuples, variables (for c: are variables just 0-elem arrays?), functions

}

trait Impl extends BaseExp with DSL {


  case class Plus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Minus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Time(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Div(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Mod(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]

  case class Int(e: Exp[scala.Int]) extends IntOps {
    def +(y: Int) = Int(Plus(e, y.e))
    def -(y: Int) = Int(Minus(e, y.e))
    def *(y: Int) = Int(Time(e, y.e))
    def /(y: Int) = Int(Div(e, y.e))
    def %(y: Int) = Int(Mod(e, y.e))
  }

  case class And(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class Or(e1: Exp[scala.Boolean], e2: Exp[scala.Boolean]) extends Def[scala.Boolean]
  case class Not(e1: Exp[scala.Boolean]) extends Def[scala.Boolean]

  case class Boolean(e: Exp[scala.Boolean]) extends BooleanOps {
    def &&(y: => Boolean) = Boolean(And(e, y.e))
    def ||(y: => Boolean) = Boolean(Or(e, y.e))
    def unary_! = Boolean(Not(e))
  }

  case class Unit(e: Exp[scala.Unit])

  implicit val unitTyp: Rep[Unit] = new Rep[Unit] { type U = scala.Unit; def from(e:Exp[U]) = Unit(e); def to(x:Unit) = x.e; def m = manifest[U]; override def toString = "Unit" }
  implicit val intTyp: Rep[Int] = new Rep[Int] {  type U = scala.Int; def from(e:Exp[U]) = Int(e); def to(x:Int) = x.e; def m = manifest[U]; override def toString = "Int" }
  implicit val booleanTyp: Rep[Boolean] = new Rep[Boolean] { type U = scala.Boolean; def from(e:Exp[U]) = Boolean(e); def to(x:Boolean) = x.e; def m = manifest[U]; override def toString = "Boolean" }

  implicit val intLift: Lift[scala.Int,Int] = new Lift[scala.Int,Int] { def to(x:scala.Int) = Int(unit(x)) }
  implicit val booleanLift: Lift[scala.Boolean,Boolean] = new Lift[scala.Boolean,Boolean] { def to(x:scala.Boolean) = Boolean(unit(x)) }


  case class ArrayLength[T](e1: Exp[scala.Array[T]]) extends Def[scala.Int]
  case class ArrayNew[T](e1: Exp[scala.Int]) extends Def[scala.Array[T]]
  case class ArrayApply[T](e1: Exp[scala.Array[T]], e2: Exp[scala.Int]) extends Def[T]
  case class ArrayUpdate[T](e1: Exp[scala.Array[T]], e2: Exp[scala.Int], e3:Exp[T]) extends Def[scala.Unit]


  case class Array[T:Rep](bleh: Exp[scala.Array[Any]]) extends ArrayOps[T] {
    val tp = typ[T]
    val e = bleh.asInstanceOf[Exp[scala.Array[tp.U]]]
    implicit val mf = tp.m
    def length = Int(ArrayLength(e))
    def apply(x: Int) = tp.from(toAtom(ArrayApply(e, x.e)))
    def update(x: Int, y: T): Unit = Unit(ArrayUpdate(e, x.e, tp.to(y)))
  }

  def NewArray[T:Rep](x: Int): Array[T] = {
    val tp = typ[T]
    //    implicit val tpm = tp.m
    val an: Exp[scala.Array[Any]]  = toAtom(ArrayNew(x.e))
    Array(an)
  }

  implicit def arrayTyp[T:Rep]: Rep[Array[T]] = new Rep[Array[T]] {
    val tp = typ[T]
    type U = scala.Array[tp.U]
    private implicit val tpm = tp.m
    def from(e:Exp[U]) = Array(e.asInstanceOf[Exp[scala.Array[Any]]]);
    def to(x:Array[T]) = x.e.asInstanceOf[Exp[U]]
    def m = manifest[U]
    override def toString = "Array["+typ[T]+"]"
  }

 /* def __ifThenElse[C,A,B](c:Boolean, a: =>A, b: =>B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Rep[C]): C = {
    reflect[C]("if (",ref(c),") ",ref(mA.to(a))," else ",ref(mB.to(b)))
  }
  */
}
