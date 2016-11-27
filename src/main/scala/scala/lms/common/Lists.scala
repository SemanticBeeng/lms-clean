package scala.lms
package common

import scala.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}

trait Lists extends Base {
  this: Ints with Units =>

  type List[T] <: ListOps[T]

  def NewList[T:Rep](x: Int): List[T]

  implicit def listRep[T:Rep]: Rep[List[T]] 
  implicit def listLift[T:Rep]: Lift[scala.List[T], List[T]]  

  trait ListOps[T] {
    def length: Int
    def apply(x: Int): T
  }

}



trait ListsExp extends BaseExp with Lists {
  this: IntsExp with UnitsExp =>


  case class ListLength[T](e1: Exp[scala.List[T]]) extends Def[scala.Int]
  case class ListNew[T](e1: Exp[scala.Int]) extends Def[scala.List[T]]
  case class ListApply[T](e1: Exp[scala.List[T]], e2: Exp[scala.Int]) extends Def[T]

  case class List[T:Rep](bleh: Exp[scala.List[Any]]) extends ListOps[T] with Expressable[scala.List[Any]] {
    val tp = rep[T]
    val e:Exp[scala.List[tp.Internal]] = bleh.asInstanceOf[Exp[scala.List[tp.Internal]]]
    implicit val mf = tp.m
    def length = int(list_length(e))
    def apply(x: Int) = tp.from(list_apply(e, x.e))
  }

/*  private def listRepF[T: Rep] = {
    repF[T, scala.List, List](list)
  }
  def listLift[T:Rep] = listRepF[T]
 */

  implicit def listRep[T](implicit tp: Rep[T]) = new Rep[List[T]] {
    type Internal = scala.List[tp.Internal]
    implicit val tpm = tp.m
    def from(e:Exp[Internal]) = list(e.asInstanceOf[Exp[scala.List[Any]]]);
    def to(x:List[T]) = {
      x.e.asInstanceOf[Exp[Internal]]
    }
    def m = manifest[Internal]
    override def toString = "List["+rep[T]+"]"
  }

  def listLift[T:Rep] = new Lift[scala.List[T], List[T]] {
    val repT = rep[T]
    def lift(l: scala.List[T]) = {
      implicit val m = repT.m
      val r:scala.List[Exp[repT.Internal]] = l.map(x => repT.to(x) )
      val r2 = list[T](unit(r))
      r2
    }
  }

  def list[T:Rep](x: Exp[scala.List[Any]]) = List[T](x)

  def NewList[T:Rep](x: Int) = {
    val tp = rep[T]
    implicit val tpm = tp.m
    list[T](ListNew[tp.Internal](x.e))
  }

  def list_apply[T:Manifest](e1: Exp[scala.List[T]], e2: Exp[scala.Int]): Exp[T]
  def list_length[T](e1: Exp[scala.List[T]]): Exp[scala.Int]
  
}

trait ListsImpl extends ListsExp  {
  this: IntsExp with UnitsExp =>

  def list_apply[T:Manifest](e1: Exp[scala.List[T]], e2: Exp[scala.Int]) = toAtom(ListApply[T](e1, e2))
  def list_length[T](e1: Exp[scala.List[T]]) = ListLength(e1)    

}


trait ListsOptImpl extends ListsImpl with EffectExp{
  this: IntsExp with UnitsExp =>


}


trait BaseGenLists extends GenericNestedCodegen {
  import IR._

}

trait ScalaGenLists extends BaseGenLists with ScalaGenNested {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    /*
    case ListNew(xs) => emitValDef(sym, src"List(${(xs map {quote}).mkString(",")})")

    case ListConcat(xs,ys) => emitValDef(sym, src"$xs ::: $ys")
    case ListCons(x, xs) => emitValDef(sym, src"$x :: $xs")
    case ListHead(xs) => emitValDef(sym, src"$xs.head")
    case ListTail(xs) => emitValDef(sym, src"$xs.tail")
    case ListIsEmpty(xs) => emitValDef(sym, src"$xs.isEmpty")
    case ListFromSeq(xs) => emitValDef(sym, src"List($xs: _*)")
    case ListMkString(xs) => emitValDef(sym, src"$xs.mkString")
    case ListMkString2(xs,s) => emitValDef(sym, src"$xs.mkString($s)")
    case ListMap(l,x,blk) => 
      gen"""val $sym = $l.map { $x => 
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case ListFlatMap(l, x, b) =>
      gen"""val $sym = $l.flatMap { $x => 
           |${nestedBlock(b)}
           |$b
           |}"""
    case ListFilter(l, x, b) =>
      gen"""val $sym = $l.filter { $x => 
           |${nestedBlock(b)}
           |$b
           |}"""
    case ListSortBy(l,x,blk) =>
      gen"""val $sym = $l.sortBy { $x => 
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case ListPrepend(l,e) => emitValDef(sym, src"$e :: $l")    
    case ListToArray(l) => emitValDef(sym, src"$l.toArray")
    case ListToSeq(l) => emitValDef(sym, src"$l.toSeq")
     */
    case _ => super.emitNode(sym, rhs) 
  }
}
