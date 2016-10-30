package scala.lms

import internal._

import scala.reflect.SourceContext

import scala.annotation.implicitNotFound


trait Base {
  // preliminaries
  @implicitNotFound("${T} is not a DSL type")
  type Exp[+T]

  trait Lift[A,B] {
    def to(x:A):B
  }

  implicit def identLift[T:Rep]: Lift[T,T] = new Lift[T,T] { def to(x:T) = x }
  implicit def lift[T,U](x:T)(implicit e: Lift[T,U], rep:Rep[U]): U = e.to(x)

  trait Rep[T] {
    type U
    def from(e:Exp[U]): T
    def to(x:T):Exp[U]
    def m: Manifest[U]
  }

/*  case class Rewrite[T:Rep](a:T, b:T)

  def lower[A:Rep,B:Rep,C:Rep](f: (A,B) => Rewrite[C]): Unit
 */
}

trait BaseExp extends Base with Expressions with Blocks with Transforming {

  def typ[T:Rep] = implicitly[Rep[T]]

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


trait ScalaGenBase extends ScalaCodegen
trait ScalaGenNested extends ScalaNestedCodegen with ScalaGenBase

trait ScalaGenFat extends ScalaFatCodegen with ScalaGenBase
