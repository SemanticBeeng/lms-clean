package scala.lms
package common

import simulacrum._

trait Ints2 extends Base {

  @typeclass trait Num[A]{
    @op("+") def plus(x:A, y: A): A
  }

  @typeclass trait IntOps[A] extends Num[A]{
    def plus(x:A, y: A): A
    @op("-") def minus(x:A, y: A): A    
  }

  type Int  

  implicit def intStagedOps: IntOps[scala.Int]
  implicit def intBaseOps: IntOps[Int]
  implicit def intRep: Rep[Int] { type Internal = scala.Int }
  implicit def intLift: Lift[scala.Int,Int]

}

trait Ints2Impl extends Ints2 with BaseExp {

  case class Plus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Minus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]

  case class Int(e: Exp[scala.Int]) extends Expressable[scala.Int] 

  val intStagedOps: IntOps[scala.Int] = new IntOps[scala.Int] {
    def plus(x:scala.Int, y:scala.Int) = x + y
    def minus(x:scala.Int, y:scala.Int) = x - y
  }

  val intBaseOps: IntOps[Int] = new IntOps[Int] {
    def plus(x:Int, y:Int) = Int(Plus(x.e, y.e))
    def minus(x:Int, y:Int) = Int(Minus(x.e, y.e))
  }

  private val repE = RepE[scala.Int, Int](Int)
  val intRep: Rep[Int] { type Internal = scala.Int } = repE
  val intLift: Lift[scala.Int,Int] = repE
  
}

trait Ints extends Base {

  trait IntOps[A] {
    def +(y: A): A
    def -(y: A): A
    def *(y: A): A
    def /(y: A): A
    def %(y: A): A
  }

  implicit def intTyp: Rep[Int] { type Internal = scala.Int }
  implicit def intLift: Lift[scala.Int,Int]
//  implicit def intOps(

  type Int <: IntOps[Int]

}

trait IntsImpl extends BaseExp with Ints {

  case class Plus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Minus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Times(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Div(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Mod(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]

  case class Int(e: Exp[scala.Int]) extends IntOps[Int] with Expressable[scala.Int] {
    def +(y: Int) = Int(Plus(e, y.e))
    def -(y: Int) = Int(Minus(e, y.e))
    def *(y: Int) = Int(Times(e, y.e))
    def /(y: Int) = Int(Div(e, y.e))
    def %(y: Int) = Int(Mod(e, y.e))
  }

  //  implicit val intTyp: Rep[Int] { type U = scala.Int } = new Rep[Int] {  type U = scala.Int; def from(e:Exp[U]) = Int(e); def to(x:Int) = x.e; def m = manifest[U]; override def toString = "Int" }
  private val repE = RepE[scala.Int, Int](Int)
  implicit val intTyp: Rep[Int] { type Internal = scala.Int } = repE
  implicit val intLift: Lift[scala.Int,Int] = repE

}

trait ScalaGenInts extends ScalaGenBase {
  val IR: IntsImpl
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Plus(a,b) =>  emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case Minus(a,b) => emitValDef(sym, "" + quote(a) + "-" + quote(b))
    case Times(a,b) => emitValDef(sym, "" + quote(a) + "*" + quote(b))
    case Div(a,b) =>   emitValDef(sym, "" + quote(a) + "/" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }

}
