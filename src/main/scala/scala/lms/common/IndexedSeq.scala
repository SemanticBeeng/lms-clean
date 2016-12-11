package scala.lms
package common

import scala.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}


trait IndexedSeqs extends Base {
  this: Ints with Units =>

  type IndexedSeq[T] <: IndexedSeqOps[T]

  def NewIndexedSeq[T:Rep](x: T*): IndexedSeq[T]


  implicit def indexedSeqRep[T:Rep]: Rep[IndexedSeq[T]]
  implicit def indexedSeqLift[U, T](implicit tp: Rep[T], lift: Lift[U, T]) : Lift[scala.IndexedSeq[U], IndexedSeq[T]]
  implicit def indexedSeqLiftIdent[T](implicit tp: Rep[T]) : Lift[scala.IndexedSeq[T], IndexedSeq[T]]   

  trait IndexedSeqOps[T] {
    def length: Int
    def apply(x: Int): T
    def toScalaIndexedSeq(size: scala.Int): scala.IndexedSeq[T]
  }
  

}



trait IndexedSeqsExp extends BaseExp with IndexedSeqs {
  this: IntsExp with UnitsExp =>


  case class IndexedSeqLength[T](e1: Exp[scala.IndexedSeq[T]]) extends Def[scala.Int]
  case class IndexedSeqNew[T](e: Exp[T]*) extends Def[scala.IndexedSeq[T]]
  case class IndexedSeqApply[T](e1: Exp[scala.IndexedSeq[T]], e2: Exp[scala.Int]) extends Def[T]

  case class IndexedSeq[T:Rep](e: Exp[scala.IndexedSeq[Any]]) extends IndexedSeqOps[T] with Expressable[scala.IndexedSeq[Any]] {
    val tp = rep[T]
    implicit val mf = tp.m

    type U = tp.Internal
    val typedE:Exp[scala.IndexedSeq[U]] = e.asInstanceOf[Exp[scala.IndexedSeq[U]]]

    def length = int(indexedSeq_length(e))
    def apply(x: Int) = tp.from(indexedSeq_apply(typedE, x.e))
    def toScalaIndexedSeq(size: scala.Int) = (0 until size).map(x => apply(int(x))).toIndexedSeq
  }


  implicit def indexedSeqRep[T](implicit tp: Rep[T]) = {
    implicit val tpm = tp.m
    repF[T, scala.IndexedSeq, IndexedSeq](tp)(indexedSeq(_), manifest[scala.IndexedSeq[tp.Internal]])
  }


  def indexedSeqLift[U,T](implicit tp: Rep[T], liftInner: Lift[U,T]) = new Lift[scala.IndexedSeq[U], IndexedSeq[T]] {
    def lift(l: scala.IndexedSeq[U]) = NewIndexedSeq[T](l.map(x => liftInner.lift(x)):_*)
  }

  def indexedSeqLiftIdent[T](implicit tp: Rep[T]) = indexedSeqLift(tp, identLift)
  
  
  def indexedSeq[T:Rep](x: Exp[scala.IndexedSeq[Any]]) = IndexedSeq[T](x)

  def NewIndexedSeq[T:Rep](x: T*) = {
    val tp = rep[T]
    implicit val tpm = tp.m
    val l: Exp[scala.IndexedSeq[tp.Internal]] = IndexedSeqNew[tp.Internal](x.map(tp.to(_)):_*)
    indexedSeq[T](l.asInstanceOf[Exp[scala.IndexedSeq[Any]]])
  }

  def indexedSeq_apply[T:Manifest](e1: Exp[scala.IndexedSeq[T]], e2: Exp[scala.Int]): Exp[T]
  def indexedSeq_length[T](e1: Exp[scala.IndexedSeq[T]]): Exp[scala.Int]
  
}

trait IndexedSeqsImpl extends IndexedSeqsExp  {
  this: IntsExp with UnitsExp =>

  def indexedSeq_apply[T:Manifest](e1: Exp[scala.IndexedSeq[T]], e2: Exp[scala.Int]) = IndexedSeqApply[T](e1, e2)
  def indexedSeq_length[T](e1: Exp[scala.IndexedSeq[T]]) = IndexedSeqLength(e1)    

}


trait IndexedSeqsOptImpl extends IndexedSeqsImpl with EffectExp{
  this: IntsExp with UnitsExp =>


}


trait BaseGenIndexedSeqs extends GenericNestedCodegen {
  import IR._

}

trait ScalaGenIndexedSeqs extends BaseGenIndexedSeqs with ScalaGenNested {
  val IR: RichExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    /*
    case IndexedSeqNew(xs) => emitValDef(sym, src"IndexedSeq(${(xs map {quote}).mkString(",")})")

    case IndexedSeqConcat(xs,ys) => emitValDef(sym, src"$xs ::: $ys")
    case IndexedSeqCons(x, xs) => emitValDef(sym, src"$x :: $xs")
    case IndexedSeqHead(xs) => emitValDef(sym, src"$xs.head")
    case IndexedSeqTail(xs) => emitValDef(sym, src"$xs.tail")
    case IndexedSeqIsEmpty(xs) => emitValDef(sym, src"$xs.isEmpty")
    case IndexedSeqFromSeq(xs) => emitValDef(sym, src"IndexedSeq($xs: _*)")
    case IndexedSeqMkString(xs) => emitValDef(sym, src"$xs.mkString")
    case IndexedSeqMkString2(xs,s) => emitValDef(sym, src"$xs.mkString($s)")
    case IndexedSeqMap(l,x,blk) => 
      gen"""val $sym = $l.map { $x => 
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case IndexedSeqFlatMap(l, x, b) =>
      gen"""val $sym = $l.flatMap { $x => 
           |${nestedBlock(b)}
           |$b
           |}"""
    case IndexedSeqFilter(l, x, b) =>
      gen"""val $sym = $l.filter { $x => 
           |${nestedBlock(b)}
           |$b
           |}"""
    case IndexedSeqSortBy(l,x,blk) =>
      gen"""val $sym = $l.sortBy { $x => 
           |${nestedBlock(blk)}
           |$blk
           |}"""
    case IndexedSeqPrepend(l,e) => emitValDef(sym, src"$e :: $l")    
    case IndexedSeqToArray(l) => emitValDef(sym, src"$l.toArray")
    case IndexedSeqToSeq(l) => emitValDef(sym, src"$l.toSeq")
     */
    case IndexedSeqNew(xs@_*) => emitValDef(sym, src"IndexedSeq(${(xs map {quote}).mkString(",")})")    
    case IndexedSeqApply(l, e2) => emitValDef(sym, src"$l($e2)")    
    case _ => super.emitNode(sym, rhs) 
  }
}
