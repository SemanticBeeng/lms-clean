package scala.lms
package common

import scala.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}


trait Matrixs extends Base {
  this: Ints with Units =>


  type Matrix[T <: Num[T]] <: MatrixOps[T]

  def NewMatrix[T <:Num[T]](h: Int, w:Int, ts: T*)(implicit rep: Rep[T]): Matrix[T]  
  def NewMatrixZeros[T <:Num[T]](hw: (Int, Int))(implicit rep: Rep[T]): Matrix[T]
  def NewMatrixOnes[T <: Num[T]](hw: (Int, Int))(implicit rep: Rep[T]): Matrix[T]  

  type ScalaMatrix[T] = IndexedSeq[IndexedSeq[T]]
  

  implicit def matrixRep[T <: Num[T]](implicit rep: Rep[T]): Rep[Matrix[T]]
  implicit def matrixLift[U, T <: Num[T]](implicit tp: Rep[T], lift: Lift[U, T]) : Lift[ScalaMatrix[U], Matrix[T]]
  implicit def matrixLiftIdent[T <: Num[T]](implicit tp: Rep[T]) : Lift[ScalaMatrix[T], Matrix[T]]   

  trait MatrixOps[T <: Num[T]] extends AddTimeAble[Matrix[T]]{
    self: Matrix[T] =>    

    def apply(x:Int): T
    def height: Int
    def width: Int
    def *(y:Matrix[T]): Matrix[T]
    def +(y:Matrix[T]): Matrix[T]
    def -(y:Matrix[T]): Matrix[T]
    def vstack(y:Matrix[T]): Matrix[T]
    def hstack(y:Matrix[T]): Matrix[T]
    def length: Int
  }




}



trait MatrixsExp extends BaseExp with Matrixs {
  this: IntsExp with UnitsExp =>


  case class MatrixHeight[T](e1: Exp[ScalaMatrix[T]]) extends Def[scala.Int]
  case class MatrixWidth[T](e1: Exp[ScalaMatrix[T]]) extends Def[scala.Int]
  case class MatrixLength[T](e1: Exp[ScalaMatrix[T]]) extends Def[scala.Int]      
  case class MatrixNewZeros[T](h: Exp[scala.Int], w: Exp[scala.Int]) extends Def[ScalaMatrix[T]]
  case class MatrixNewOnes[T](h: Exp[scala.Int], w: Exp[scala.Int]) extends Def[ScalaMatrix[T]]
  case class MatrixNew[T](h: Exp[scala.Int], w: Exp[scala.Int], e: Exp[T]*) extends Def[ScalaMatrix[T]]  
  case class MatrixTimes[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]]) extends Def[ScalaMatrix[T]]
  case class MatrixAdd[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]]) extends Def[ScalaMatrix[T]]
  case class MatrixMinus[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]]) extends Def[ScalaMatrix[T]]
  case class MatrixHStack[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]]) extends Def[ScalaMatrix[T]]
  case class MatrixVStack[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]]) extends Def[ScalaMatrix[T]]
  case class MatrixApply[T](e1: Exp[ScalaMatrix[T]], e2: Exp[scala.Int]) extends Def[T]

  case class Matrix[T <: Num[T]](e: Exp[ScalaMatrix[Any]])(implicit repT: Rep[T]) extends MatrixOps[T] with Expressable[ScalaMatrix[Any]] {

    val tp = repT
    implicit val mf = tp.m

    type U = tp.Internal
    val typedE:Exp[ScalaMatrix[U]] = e.asInstanceOf[Exp[ScalaMatrix[U]]]

    def conv(x:Exp[Any]) = x.asInstanceOf[Exp[ScalaMatrix[U]]]
    def length = int(matrix_length(e))
    def width = int(matrix_width(e))
    def height = int(matrix_height(e))        
    def apply(x: Int): T = tp.from(matrix_apply(typedE, x.e))
    def +(x: Matrix[T]) = matrix[T](matrix_plus[U](typedE, conv(x.typedE)))
    def *(x: Matrix[T]) = matrix[T](matrix_times[U](typedE, conv(x.typedE)))
    def -(x: Matrix[T]) = matrix[T](matrix_minus[U](typedE, conv(x.typedE)))
    def hstack(x: Matrix[T]) = matrix[T](matrix_hstack[U](typedE, conv(x.typedE)))
    def vstack(x: Matrix[T]) = matrix[T](matrix_vstack[U](typedE, conv(x.typedE)))

  }


  implicit def matrixRep[T <: Num[T]](implicit tp: Rep[T]) = new Rep[Matrix[T]]{
    private implicit val tpm = tp.m    
    type Internal = ScalaMatrix[tp.Internal]
    def from(x:Exp[Internal]) = matrix(x.asInstanceOf[Exp[ScalaMatrix[Any]]])
    def to(x:Matrix[T]) = x.e.asInstanceOf[Exp[Internal]]
    def m = manifest[Internal]
  }


  def matrixLift[U,T <: Num[T]](implicit tp: Rep[T], liftInner: Lift[U,T]) = new Lift[ScalaMatrix[U], Matrix[T]] {
    def lift(l: ScalaMatrix[U]) = NewMatrix[T](intLift.lift(1), intLift.lift(l.length), l.flatten.map(x => liftInner.lift(x)):_*)
  }

  def matrixLiftIdent[T <: Num[T]](implicit tp: Rep[T]) = matrixLift(tp, identLift)
  
  
  def matrix[T <: Num[T]](x: Exp[ScalaMatrix[Any]])(implicit tp: Rep[T]) = {
    Matrix[T](x)    
  }

  def NewMatrix[T <: Num[T]](h:Int, w: Int, x: T*)(implicit tp: Rep[T]) = {
    implicit val tpm = tp.m
    val l: Exp[ScalaMatrix[tp.Internal]] = MatrixNew[tp.Internal](intRep.to(h), intRep.to(w), x.map(tp.to(_)):_*)
    matrix[T](l.asInstanceOf[Exp[ScalaMatrix[Any]]])
  }
  
  def NewMatrixZeros[T <: Num[T]](hw: (Int, Int))(implicit tp: Rep[T]) = {
    val (h, w) = hw    
    implicit val tpm = tp.m
    val l: Exp[ScalaMatrix[tp.Internal]] = MatrixNewZeros[tp.Internal](w.e, h.e)
    matrix[T](l.asInstanceOf[Exp[ScalaMatrix[Any]]])
  }

  def NewMatrixOnes[T <: Num[T]](hw: (Int,Int))(implicit tp: Rep[T]) = {
    val (h, w) = hw
    implicit val tpm = tp.m
    val l: Exp[ScalaMatrix[tp.Internal]] = MatrixNewOnes[tp.Internal](w.e, h.e)
    matrix[T](l.asInstanceOf[Exp[ScalaMatrix[Any]]])
  }


  def matrix_apply[T:Manifest](e1: Exp[ScalaMatrix[T]], e2: Exp[scala.Int]): Exp[T]
  def matrix_length[T](e1: Exp[ScalaMatrix[T]]): Exp[scala.Int]
  def matrix_width[T](e1: Exp[ScalaMatrix[T]]): Exp[scala.Int]
  def matrix_height[T](e1: Exp[ScalaMatrix[T]]): Exp[scala.Int]    
  def matrix_plus[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]): Exp[ScalaMatrix[T]]
  def matrix_minus[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]): Exp[ScalaMatrix[T]]
  def matrix_times[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]): Exp[ScalaMatrix[T]]
  def matrix_hstack[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]): Exp[ScalaMatrix[T]]
  def matrix_vstack[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]): Exp[ScalaMatrix[T]]
 


}

  

trait MatrixsImpl extends MatrixsExp  {
  this: IntsExp with UnitsExp =>

  def matrix_apply[T:Manifest](e1: Exp[ScalaMatrix[T]], e2: Exp[scala.Int]) = MatrixApply[T](e1, e2)
  def matrix_length[T](e1: Exp[ScalaMatrix[T]]) = MatrixLength(e1)
  def matrix_width[T](e1: Exp[ScalaMatrix[T]]) = MatrixWidth(e1)
  def matrix_height[T](e1: Exp[ScalaMatrix[T]]) = MatrixHeight(e1)    
  def matrix_plus[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]) = MatrixAdd[T](e1, e2)
  def matrix_minus[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]) = MatrixMinus[T](e1, e2)
  def matrix_times[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]) = MatrixTimes[T](e1, e2)
  def matrix_hstack[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]) = MatrixHStack[T](e1, e2)
  def matrix_vstack[T](e1: Exp[ScalaMatrix[T]], e2: Exp[ScalaMatrix[T]])(implicit m: Manifest[ScalaMatrix[T]]) = MatrixVStack[T](e1, e2)        


}


trait MatrixsOptImpl extends MatrixsImpl with EffectExp{
  this: IntsExp with UnitsExp =>


}


trait BaseGenMatrixs extends GenericNestedCodegen {
  import IR._

}

trait ScalaGenMatrixs extends BaseGenMatrixs with ScalaGenNested {
  val IR: RichExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MatrixAdd(m1, m2) => emitValDef(sym, src"{val l = $m1(0).length; $m1.flatten.zip($m2.flatten).map(x => (x._1+x._2)).sliding(l, l).toIndexedSeq}")      
    case MatrixMinus(m1, m2) => emitValDef(sym, src"{val l = $m1(0).length; $m1.flatten.zip($m2.flatten).map(x => (x._1-x._2)).sliding(l, l).toIndexedSeq}")      
    case MatrixTimes(m1, m2) => emitValDef(sym, src"""{val a = $m1.length; val b = $m1(0).length; val c = $m2.length; val d = $m2(0).length; val ar = Array.fill(a, d)(0); for (i <- (0 until a)) for (j <- (0 until d)) for (k <- (0 until c)) ar(i)(j) += $m1(i)(k)*$m2(k)(j); ar.map(_.toIndexedSeq).toIndexedSeq}""")
    case MatrixHStack(m1, m2) => emitValDef(sym, src"$m1 ++ $m2")
    case MatrixVStack(m1, m2) => emitValDef(sym, src"$m1.zip($m2).map(x => x._1 ++ x._2)")            
    case MatrixNew(h, w, xs@_*) => emitValDef(sym, src"IndexedSeq(${(xs map {quote}).mkString(",")}).sliding($w, $w).toIndexedSeq")
    case MatrixLength(m) => emitValDef(sym, src"$m.length*$m(0).length")                        
    case MatrixWidth(m) => emitValDef(sym, src"$m(0).length")                  
    case MatrixHeight(m) => emitValDef(sym, src"$m.length")            
    case MatrixNewZeros(h, w) => emitValDef(sym, src"IndexedSeq.fill($h, $w)(0)")
    case MatrixNewOnes(h, w) => emitValDef(sym, src"IndexedSeq.fill($h, $w)(0)")
    case MatrixApply(l, e2) => emitValDef(sym, src"$l($e2)")    
    case _ => super.emitNode(sym, rhs) 
  }
}
