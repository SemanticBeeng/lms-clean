package scala.lms
package common


trait Arrays extends Base {
  this: Ints with Units =>

  type Array[T] <: ArrayOps[T]
  def NewArray[T:Rep](x: Int): Array[T]
  implicit def arrayTyp[T:Rep]: Rep[Array[T]]


  trait ArrayOps[T] {
    def length: Int
    def apply(x: Int): T
    def update(x: Int, y: T): Unit
  }

}

trait ArraysImpl extends BaseExp with Arrays {
  this: IntsImpl with UnitsImpl =>


//  def __ifThenElse[C,A,B](c:Boolean, a: =>A, b: =>B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Rep[C]): C
// tuples, variables (for c: are variables just 0-elem arrays?), functions
 /* def __ifThenElse[C,A,B](c:Boolean, a: =>A, b: =>B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Rep[C]): C = {
    reflect[C]("if (",ref(c),") ",ref(mA.to(a))," else ",ref(mB.to(b)))
  }
  */


  case class ArrayLength[T](e1: Exp[scala.Array[T]]) extends Def[scala.Int]
  case class ArrayNew[T](e1: Exp[scala.Int]) extends Def[scala.Array[T]]
  case class ArrayApply[T](e1: Exp[scala.Array[T]], e2: Exp[scala.Int]) extends Def[T]
  case class ArrayUpdate[T](e1: Exp[scala.Array[T]], e2: Exp[scala.Int], e3:Exp[T]) extends Def[scala.Unit]


  case class Array[T:Rep](bleh: Exp[scala.Array[Any]]) extends ArrayOps[T] {
    val tp = typ[T]
    val e = bleh.asInstanceOf[Exp[scala.Array[tp.Internal]]]
    implicit val mf = tp.m
    def length = Int(ArrayLength(e))
    def apply(x: Int) = tp.from(toAtom(ArrayApply(e, x.e)))
    def update(x: Int, y: T): Unit = Unit(ArrayUpdate(e, x.e, tp.to(y)))
  }

  def NewArray[T:Rep](x: Int): Array[T] = {
    val tp = typ[T]
    val an: Exp[scala.Array[Any]]  = toAtom(ArrayNew(x.e))
    Array(an)
  }

  implicit def arrayTyp[T:Rep]: Rep[Array[T]] = new Rep[Array[T]] {
    val tp = typ[T]
    type Internal = scala.Array[tp.Internal]
    private implicit val tpm = tp.m
    def from(e:Exp[Internal]) = Array(e.asInstanceOf[Exp[scala.Array[Any]]]);
    def to(x:Array[T]) = x.e.asInstanceOf[Exp[Internal]]
    def m = manifest[Internal]
    override def toString = "Array["+typ[T]+"]"
  }

}
