package scala.lms
package common

import scala.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}


trait Arrays extends Base {
  this: Ints with Units =>

  type Array[T] <: ArrayOps[T]

  def NewArray[T:Rep](x: T*): Array[T]

  implicit def arrayRep[T:Rep]: Rep[Array[T]]
  implicit def arrayLift[U, T](implicit tp: Rep[T], lift: Lift[U, T]) : Lift[scala.Array[U], Array[T]]
  implicit def arrayLiftIdent[T](implicit tp: Rep[T]) : Lift[scala.Array[T], Array[T]]   

  trait ArrayOps[T] {
    def length: Int
    def apply(x: Int): T
  }
  

}



trait ArraysExp extends BaseExp with Arrays {
  this: IntsExp with UnitsExp =>


  case class ArrayLength[T](e1: Exp[scala.Array[T]]) extends Def[scala.Int]
  case class ArrayNew[T](e: Exp[T]*) extends Def[scala.Array[T]]
  case class ArrayApply[T](e1: Exp[scala.Array[T]], e2: Exp[scala.Int]) extends Def[T]

  case class Array[T:Rep](e: Exp[scala.Array[Any]]) extends ArrayOps[T] with Expressable[scala.Array[Any]] {

    val tp = rep[T]
    implicit val mf = tp.m

    type U = tp.Internal
    val typedE:Exp[scala.Array[U]] = e.asInstanceOf[Exp[scala.Array[U]]]

    def length = int(array_length(e))
    def apply(x: Int) = tp.from(array_apply(typedE, x.e))
    
  }


  implicit def arrayRep[T](implicit tp: Rep[T]) = {
    implicit val tpm = tp.m
    repF[T, scala.Array, Array](tp)(array(_), manifest[scala.Array[tp.Internal]])
  }


  def arrayLift[U,T](implicit tp: Rep[T], liftInner: Lift[U,T]) = new Lift[scala.Array[U], Array[T]] {
    def lift(l: scala.Array[U]) = NewArray[T](l.map(x => liftInner.lift(x)):_*)
  }

  def arrayLiftIdent[T](implicit tp: Rep[T]) = arrayLift(tp, identLift)
  
  
  def array[T:Rep](x: Exp[scala.Array[Any]]) = Array[T](x)

  def NewArray[T:Rep](x: T*) = {
    val tp = rep[T]
    implicit val tpm = tp.m
    val l: Exp[scala.Array[tp.Internal]] = ArrayNew[tp.Internal](x.map(tp.to(_)):_*)
    array[T](l.asInstanceOf[Exp[scala.Array[Any]]])
   
  }

  def array_apply[T:Manifest](e1: Exp[scala.Array[T]], e2: Exp[scala.Int]): Exp[T]
  def array_length[T](e1: Exp[scala.Array[T]]): Exp[scala.Int]
  
}

trait ArraysImpl extends ArraysExp  {
  this: IntsExp with UnitsExp =>

  def array_apply[T:Manifest](e1: Exp[scala.Array[T]], e2: Exp[scala.Int]) = ArrayApply[T](e1, e2)
  def array_length[T](e1: Exp[scala.Array[T]]) = ArrayLength(e1)    

}


trait ArraysOptImpl extends ArraysImpl with EffectExp{
  this: IntsExp with UnitsExp =>


}


trait BaseGenArrays extends GenericNestedCodegen {
  import IR._

}

trait ScalaGenArrays extends BaseGenArrays with ScalaGenNested {
  val IR: RichExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    /*
    case ArrayNew(xs) => emitValDef(sym, src"Array(${(xs map {quote}).mkString(",")})")

    case ArrayConcat(xs,ys) => emitValDef(sym, src"$xs ::: $ys")
    case ArrayCons(x, xs) => emitValDef(sym, src"$x :: $xs")
    case ArrayHead(xs) => emitValDef(sym, src"$xs.head")
    case ArrayTail(xs) => emitValDef(sym, src"$xs.tail")
    case ArrayIsEmpty(xs) => emitValDef(sym, src"$xs.isEmpty")
    case ArrayFromSeq(xs) => emitValDef(sym, src"Array($xs: _*)")
    case ArrayMkString(xs) => emitValDef(sym, src"$xs.mkString")
    case ArrayMkString2(xs,s) => emitValDef(sym, src"$xs.mkString($s)")
    case ArrayMap(l,x,blk) => 
      gen"""val $sym = $l.map { $x => 
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case ArrayFlatMap(l, x, b) =>
      gen"""val $sym = $l.flatMap { $x => 
           |${nestedBlock(b)}
           |$b
           |}"""
    case ArrayFilter(l, x, b) =>
      gen"""val $sym = $l.filter { $x => 
           |${nestedBlock(b)}
           |$b
           |}"""
    case ArraySortBy(l,x,blk) =>
      gen"""val $sym = $l.sortBy { $x => 
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case ArrayPrepend(l,e) => emitValDef(sym, src"$e :: $l")    
    case ArrayToArray(l) => emitValDef(sym, src"$l.toArray")
    case ArrayToSeq(l) => emitValDef(sym, src"$l.toSeq")
     */
    case ArrayNew(xs@_*) => emitValDef(sym, src"Array(${(xs map {quote}).mkString(",")})")    
    case ArrayApply(l, e2) => emitValDef(sym, src"$l($e2)")    
    case _ => super.emitNode(sym, rhs) 
  }
}
