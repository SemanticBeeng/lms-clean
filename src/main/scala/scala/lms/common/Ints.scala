package scala.lms
package common


trait Ints extends Base {

  trait IntOps {
    def +(y: Int): Int
    def -(y: Int): Int
    def *(y: Int): Int
    def /(y: Int): Int
    def %(y: Int): Int
  }

  implicit def intTyp: Rep[Int]
  implicit def intLift: Lift[scala.Int,Int]

  type Int <: IntOps

}

trait IntsImpl extends BaseExp with Ints {

  case class Plus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Minus(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Times(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Div(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]
  case class Mod(e1: Exp[scala.Int], e2: Exp[scala.Int]) extends Def[scala.Int]

  case class Int(e: Exp[scala.Int]) extends IntOps with Expressable[scala.Int] {
    def +(y: Int) = Int(Plus(e, y.e))
    def -(y: Int) = Int(Minus(e, y.e))
    def *(y: Int) = Int(Times(e, y.e))
    def /(y: Int) = Int(Div(e, y.e))
    def %(y: Int) = Int(Mod(e, y.e))
  }

  //  implicit val intTyp: Rep[Int] { type U = scala.Int } = new Rep[Int] {  type U = scala.Int; def from(e:Exp[U]) = Int(e); def to(x:Int) = x.e; def m = manifest[U]; override def toString = "Int" }
  implicit val intTyp = RepE[scala.Int, Int](Int) 
  implicit val intLift: Lift[scala.Int,Int] = intTyp

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
