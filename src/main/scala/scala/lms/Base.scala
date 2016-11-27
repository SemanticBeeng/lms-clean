package scala.lms

import internal._

import scala.reflect.SourceContext

import scala.annotation.implicitNotFound
import scala.collection.generic.FilterMonadic


trait Base {
  // preliminaries
  @implicitNotFound("${T} is not a DSL type")
  type Exp[+T]

  trait Lift[A,B] {
    def lift(x:A):B
  }

  trait LiftF[A[_],B[_]] {
    def liftF[T:Rep](x:A[T]):B[T]
  }
  
  implicit def identLift[T:Rep]: Lift[T,T] = new Lift[T,T] { def lift(x:T) = x }
  implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U = e.lift(x)
  implicit def liftF[T[V],U[V],V:Rep](x:T[V])(implicit e: LiftF[T,U]): U[V] = e.liftF(x)  

  trait Rep[T] {
    type Internal
    def from(e:Exp[Internal]): T
    def to(x:T):Exp[Internal]
    def m: Manifest[Internal]
  }



/*  case class Rewrite[T:Rep](a:T, b:T)

  def lower[A:Rep,B:Rep,C:Rep](f: (A,B) => Rewrite[C]): Unit
 */
}

trait BaseExp extends Base with Expressions with Blocks with Transforming {

  def rep[T:Rep] = implicitly[Rep[T]]


  trait Expressable[A] {
    def e: Exp[A]
  }

  case class RepE[A:Manifest,B <: Expressable[A]](f: Exp[A] => B) extends Rep[B] with Lift[A,B]{
    type Internal = A
    def from(x:Exp[A]) = f(x)
    def to(x:B) = x.e
    def lift(x:A):B = from(unit(x))
    def m = manifest[A]
  }


/*
  def repF[T: Rep] = {
    val repT = rep[T]
    type I = repT.Internal
    def rep[A[_]:Manifest, B[repT.T] <: Expressable[A[repT.T]]]()
    rep
  }
 */
  //  def repF[T, A[rep.T]:Manifest, B[rep.T] <: Expressable[A[rep.T]]](f: Exp[A[rep.T]] => B[T])(implicit rep: Rep[T]) = ()/*new Rep,] extends Rep[B[T]] with Lift[A[T],B[T]]{
  /*
    implicit val tp = rep[T]
    private implicit val tpm = tp.m    
    type Internal = A[tp.Internal]
    def from(x:Exp[Internal]) = f(x)
    def to(x:B[T]) = x.e
    def lift(x:A[T]):B[T] = from(unit(x))
    def m = manifest[A[T]]
  }*/
  
  

}

trait EffectExp extends BaseExp with Effects {


  def mapOver(t: Transformer, u: Summary) = { // TODO: move to effects class?
    u.copy(mayRead = t.onlySyms(u.mayRead), mstRead = t.onlySyms(u.mstRead),
      mayWrite = t.onlySyms(u.mayWrite), mstWrite = t.onlySyms(u.mstWrite))
  }
 
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case Reflect(x, u, es) => Reflect(mirrorDef(x,f), mapOver(f,u), f(es))
    case Reify(x, u, es) => Reify(f(x), mapOver(f,u), f(es))
    case _ =>
      super.mirrorDef(e,f)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case Reflect(x, u, es) => reflectMirrored(mirrorDef(e,f).asInstanceOf[Reflect[A]])
    case Reify(x, u, es) => Reify(f(x), mapOver(f,u), f(es))
    case _ =>
      super.mirror(e,f)

  }

}

trait BaseFatExp extends BaseExp with FatExpressions with FatTransforming

trait ScalaGenBase extends ScalaCodegen

trait ScalaGenNested extends ScalaNestedCodegen with ScalaGenBase

trait ScalaGenFat extends ScalaFatCodegen with ScalaGenBase
