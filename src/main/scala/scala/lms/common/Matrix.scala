/*package scala.lms
package common

trait MatrixOps[N] extends AddTimeAble[N]{
  self: N => 
  type I <: IntOps[I]

  def height: I
  def width: I  
  def *(y:N): N
  def +(y:N): N
  def -(y:N): N  
  def vstack(y:N): N
  def hstack(y:N): N     
}

trait Matrixs extends Base {

  this: Ints with Units =>

  type Matrix[T] <: MatrixOps[Matrix[T]] { type I = T }

  def NewMatrix[T:Rep:Num](h: Int, w: Int): Matrix[T]
  def NewMatrixOnes[T:Rep:Num](h: Int, w: Int): Matrix[T]  

  type ScalaMatrix[T] = IndexedSeq[IndexedSeq[T]]
  implicit def matrixRep[T:Rep]: Rep[Matrix[T]] 
  implicit def matrixLift[T:Rep]: Lift[ScalaMatrix[T], Matrix[T]]  


}


trait MatrixsExp extends BaseExp with Matrixs {
  this: IntsExp with UnitsExp =>

  type C[T] = Exp[T]

  case class MatrixHeight[T](e1: Exp[ScalaMatrix[C[T]]]) extends Def[scala.Int]
  case class MatrixWidth[T](e1: Exp[ScalaMatrix[C[T]]]) extends Def[scala.Int]    
  case class MatrixNew[T](h: Exp[scala.Int], w: Exp[scala.Int]) extends Def[ScalaMatrix[T]]
  case class MatrixNewOnes[T](h: Exp[scala.Int], w: Exp[scala.Int]) extends Def[ScalaMatrix[T]]
  case class MatrixTime[T](e1: Exp[ScalaMatrix[C[T]]], e2: Exp[ScalaMatrix[C[T]]]) extends Def[T]
  case class MatrixAdd[T](e1: Exp[ScalaMatrix[C[T]]], e2: Exp[ScalaMatrix[C[T]]]) extends Def[T]
  case class MatrixMinus[T](e1: Exp[ScalaMatrix[C[T]]], e2: Exp[ScalaMatrix[C[T]]]) extends Def[T]  
  case class MatrixHStack[T](e1: Exp[ScalaMatrix[C[T]]], e2: Exp[ScalaMatrix[C[T]]]) extends Def[T]
  case class MatrixVStack[T](e1: Exp[ScalaMatrix[C[T]]], e2: Exp[ScalaMatrix[C[T]]]) extends Def[T]
  case class MatrixApply[T](e1: Exp[ScalaMatrix[C[T]]], e2: Exp[scala.Int]) extends Def[T]

  case class Matrix[T:Rep](bleh: Exp[ScalaMatrix[Any]]) extends MatrixOps[T] with Expressable[ScalaMatrix[Any]] {
    type I = Int
    val tp = rep[T]
    type U = tp.Internal
    val e:Exp[ScalaMatrix[C[U]]] = bleh.asInstanceOf[Exp[ScalaMatrix[C[U]]]]
    implicit val mf = tp.m
    def length = int(matrix_length(e))
    def apply(x: Int) = tp.from(matrix_apply(e, x.e))
  }


  implicit def matrixRep[T](implicit tp: Rep[T]) = new Rep[Matrix[T]] {
    type Internal = ScalaMatrix[tp.Internal]
    implicit val tpm = tp.m
    def from(e:Exp[Internal]) = matrix(e.asInstanceOf[Exp[ScalaMatrix[Any]]]);
    def to(x:Matrix[T]) = {
      x.e.asInstanceOf[Exp[Internal]]
    }
    def m = manifest[Internal]
    override def toString = "Matrix["+rep[T]+"]"
  }

  def matrixLift[T:Rep] = new Lift[ScalaMatrix[T], Matrix[T]] {
    val repT = rep[T]
    def lift(l: ScalaMatrix[T]) = {
      implicit val m = repT.m
      val r:ScalaMatrix[C[repT.Internal]] = l.map(x => repT.to(x))
      val r2 = matrix[T](unit(r))
      println(r2.e)      
      r2
    }
  }

  def matrix[T:Rep](x: Exp[ScalaMatrix[Any]]) = Matrix[T](x)

  def NewMatrix[T:Rep](w: Int, h: Int) = {
    val tp = rep[T]
    implicit val tpm = tp.m
    matrix[T](MatrixNew[tp.Internal](w.e, h.e))
  }

  def NewMatrixOnes[T:Rep](w: Int, h: Int) = {
    val tp = rep[T]
    implicit val tpm = tp.m
    matrix[T](MatrixNewOnes[tp.Internal](w.e, h.e))
  }
  
  def matrix_apply[T:Manifest](e1: Exp[ScalaMatrix[C[T]]], e2: Exp[scala.Int]): Exp[T]
  def matrix_length[T](e1: Exp[ScalaMatrix[C[T]]]): Exp[scala.Int]
  
}
 */
