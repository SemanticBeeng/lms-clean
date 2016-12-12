package scala.lms

import internal._

import scala.reflect.SourceContext

import scala.annotation.implicitNotFound
import scala.collection.generic.FilterMonadic


trait Base {
  // preliminaries

  trait Lift[A,B] {
    def lift(x:A):B
  }
  
  implicit def identLift[T:Rep]: Lift[T,T] = new Lift[T,T] { def lift(x:T) = x }
  implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U = e.lift(x)

  type Rep[T]

}

trait BaseExp extends Base with Expressions with Blocks with Transforming {

  trait Rep[T] {
    type Internal
    def from(e:Exp[Internal]): T
    def to(x:T):Exp[Internal]
    def m: Manifest[Internal]
  }

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


  def repF[T, A[_], B[T] <: Expressable[A[Any]]](tp: Rep[T])(f: Exp[A[Any]] => B[T], man:Manifest[A[tp.Internal]]) = new Rep[B[T]]{
    private implicit val tpm = tp.m    
    type Internal = A[tp.Internal]
    def from(x:Exp[Internal]) = f(x.asInstanceOf[Exp[A[Any]]])
    def to(x:B[T]) = x.e.asInstanceOf[Exp[Internal]]
    def m = man
  }
      

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
