package scala.lms
package common

import scala.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}


trait Lists extends Base {
  this: Ints with Units =>

  type List[T] <: ListOps[T]

  def NewList[T:Rep](x: T*): List[T]


  implicit def listRep[T:Rep]: Rep[List[T]]
  implicit def listLift[U, T](implicit tp: Rep[T], lift: Lift[U, T]) : Lift[scala.List[U], List[T]]
  implicit def listLiftIdent[T](implicit tp: Rep[T]) : Lift[scala.List[T], List[T]]   

  trait ListOps[T] {
    def length: Int
    def apply(x: Int): T
    def toScalaList(size: scala.Int): scala.List[T]
  }
  

}



trait ListsExp extends BaseExp with Lists {
  this: IntsExp with UnitsExp =>


  case class ListLength[T](e1: Exp[scala.List[T]]) extends Def[scala.Int]
  case class ListNew[T](e: Exp[T]*) extends Def[scala.List[T]]
  case class ListApply[T](e1: Exp[scala.List[T]], e2: Exp[scala.Int]) extends Def[T]

  case class List[T:Rep](e: Exp[scala.List[Any]]) extends ListOps[T] with Expressable[scala.List[Any]] {
    val tp = rep[T]
    implicit val mf = tp.m

    type U = tp.Internal
    val typedE:Exp[scala.List[U]] = e.asInstanceOf[Exp[scala.List[U]]]

    def length = int(list_length(e))
    def apply(x: Int) = tp.from(list_apply(typedE, x.e))
    def toScalaList(size: scala.Int) = (0 until size).map(x => apply(int(x))).toList
  }


  implicit def listRep[T](implicit tp: Rep[T]) = {
    implicit val tpm = tp.m
    repF[T, scala.List, List](tp)(list(_), manifest[scala.List[tp.Internal]])
  }


  def listLift[U,T](implicit tp: Rep[T], liftInner: Lift[U,T]) = new Lift[scala.List[U], List[T]] {
    def lift(l: scala.List[U]) = NewList[T](l.map(x => liftInner.lift(x)):_*)
  }

  def listLiftIdent[T](implicit tp: Rep[T]) = listLift(tp, identLift)
  
  
  def list[T:Rep](x: Exp[scala.List[Any]]) = List[T](x)

  def NewList[T:Rep](x: T*) = {
    val tp = rep[T]
    implicit val tpm = tp.m
    val l: Exp[scala.List[tp.Internal]] = ListNew[tp.Internal](x.map(tp.to(_)):_*)
    list[T](l.asInstanceOf[Exp[scala.List[Any]]])
  }

  def list_apply[T:Manifest](e1: Exp[scala.List[T]], e2: Exp[scala.Int]): Exp[T]
  def list_length[T](e1: Exp[scala.List[T]]): Exp[scala.Int]
  
}

trait ListsImpl extends ListsExp  {
  this: IntsExp with UnitsExp =>

  def list_apply[T:Manifest](e1: Exp[scala.List[T]], e2: Exp[scala.Int]) = ListApply[T](e1, e2)
  def list_length[T](e1: Exp[scala.List[T]]) = ListLength(e1)    

}


trait ListsOptImpl extends ListsImpl with EffectExp{
  this: IntsExp with UnitsExp =>


}


trait BaseGenLists extends GenericNestedCodegen {
  import IR._

}

trait ScalaGenLists extends BaseGenLists with ScalaGenNested {
  val IR: RichExp
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
    case ListNew(xs@_*) => emitValDef(sym, src"List(${(xs map {quote}).mkString(",")})")    
    case ListApply(l, e2) => emitValDef(sym, src"$l($e2)")    
    case _ => super.emitNode(sym, rhs) 
  }
}
