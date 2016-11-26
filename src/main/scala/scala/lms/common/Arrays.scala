package scala.lms
package common


trait Arrays extends Base {
  this: Ints with Units =>

  type Array[T] <: ArrayOps[T]

  def NewArray[T:Rep](x: Int): Array[T]

  implicit def arrayRep[T:Rep]: Rep[Array[T]]

  trait ArrayOps[T] {
    def length: Int
    def apply(x: Int): T
    def update(x: Int, y: T): Unit
  }

}



trait ArraysExp extends BaseExp with Arrays {
  this: IntsExp with UnitsExp =>


  case class ArrayLength[T](e1: Exp[scala.Array[T]]) extends Def[scala.Int]
  case class ArrayNew[T](e1: Exp[scala.Int]) extends Def[scala.Array[T]]
  case class ArrayApply[T](e1: Exp[scala.Array[T]], e2: Exp[scala.Int]) extends Def[T]
  case class ArrayUpdate[T](e1: Exp[scala.Array[T]], e2: Exp[scala.Int], e3:Exp[T]) extends Def[scala.Unit]

  type Array[T] <: ArrayOps[T] 

  def array[T:Rep](x: Exp[scala.Array[Any]]): Array[T]

  def NewArray[T:Rep](x: Int): Array[T] = {
    val tp = rep[T]
    val an: Exp[scala.Array[Any]]  = toAtom(ArrayNew(x.e))
    array[T](an)
  }
  
  
}

trait ArraysImpl extends ArraysExp  {
  this: IntsExp with UnitsExp =>

  case class Array[T:Rep](bleh: Exp[scala.Array[Any]]) extends ArrayOps[T]  {
    val tp = rep[T]
    val e = bleh.asInstanceOf[Exp[scala.Array[tp.Internal]]]
    implicit val mf = tp.m
    def length = int(ArrayLength(e))
    def apply(x: Int) = tp.from(toAtom(ArrayApply(e, x.e)))
    def update(x: Int, y: T): Unit = unit(ArrayUpdate(e, x.e, tp.to(y)))
  }


  implicit def arrayRep[T:Rep]: Rep[Array[T]] = new Rep[Array[T]] {
    val tp = rep[T]
    type Internal = scala.Array[tp.Internal]
    private implicit val tpm = tp.m
    def from(e:Exp[Internal]) = array(e.asInstanceOf[Exp[scala.Array[Any]]]);
    def to(x:Array[T]) = x.e.asInstanceOf[Exp[Internal]]
    def m = manifest[Internal]
    override def toString = "Array["+rep[T]+"]"
  }

  def array[T:Rep](x: Exp[scala.Array[Any]]) = Array[T](x)  

}


trait ArraysOptImpl extends ArraysExp with EffectExp{
  this: IntsExp with UnitsExp =>

  case class Array[T:Rep](bleh: Exp[scala.Array[Any]]) extends ArrayOps[T] {
    val tp = rep[T]
    val e = bleh.asInstanceOf[Exp[scala.Array[tp.Internal]]]
    implicit val mf = tp.m
    def length = int(ArrayLength(e))
    def apply(x: Int) = tp.from(toAtom(ArrayApply(e, x.e)))
    def update(x: Int, y: T): Unit = unit(ArrayUpdate(e, x.e, tp.to(y)))
  }


  implicit def arrayRep[T:Rep]: Rep[Array[T]] = new Rep[Array[T]] {
    val tp = rep[T]
    type Internal = scala.Array[tp.Internal]
    private implicit val tpm = tp.m
    def from(e:Exp[Internal]) = array(e.asInstanceOf[Exp[scala.Array[Any]]]);
    def to(x:Array[T]) = x.e.asInstanceOf[Exp[Internal]]
    def m = manifest[Internal]
    override def toString = "Array["+rep[T]+"]"
  }

  def array[T:Rep](x: Exp[scala.Array[Any]]) = Array[T](x)


}
