package scala.lms
package common

import internal._

import java.io.PrintWriter


import scala.reflect.SourceContext

trait Functions extends Base {

  implicit def fun[A, B](fun: A => B)(implicit tA:Rep[A], tB:Rep[B]): Exp[tA.U => tB.U]
  //  implicit def apply[A,B](fun: A => B, A)(implicit tA:Rep[A], tB:Rep[B]): Exp[tA.U => tB.U]
  implicit def applyOps[A,B:Manifest](exp: Exp[A => B]): LambdaApply[A,B]

  trait LambdaOps[A,B] {
    def apply(a:Exp[A]):Exp[B]
  }
  type LambdaApply[A,B] <: LambdaOps[A,B]

}

trait FunctionsExp extends Functions with BaseExp with EffectExp {

  case class Lambda[A, B](f: Exp[A] => Exp[B], x: Exp[A], y: Block[B]) extends Def[A => B] 
  case class Apply[A,B](f:Exp[A => B], x:Exp[A]) extends Def[B]

  case class LambdaApply[A,B:Manifest](f:Exp[A => B]) extends LambdaOps[A,B] {
    def apply(x:Exp[A]):Exp[B] = toAtom(Apply(f, x))
  }

  def unboxedFresh[A:Manifest] : Exp[A] = fresh[A]
  def unbox[A:Manifest](x : Exp[A])(implicit pos: SourceContext) : Exp[A] = x

  def doLambdaDef[A:Manifest, B:Manifest](f: Exp[A] => Exp[B]) : Def[A => B] = {
    val x = unboxedFresh[A]
    val y = reifyEffects(f(x)) // unfold completely at the definition site.
    Lambda(f, x, y)
  }

  implicit def applyOps[A,B:Manifest](f: Exp[A => B]): LambdaApply[A,B] = LambdaApply(f)

  implicit def fun[A,B](fun: A => B)(implicit rA:Rep[A], rB:Rep[B]): Exp[rA.U => rB.U] = {
    implicit val mA = rA.m
    implicit val mB = rB.m
    implicit val m = manifest[rA.U => rB.U]
    val fA = fun.compose((x:Exp[rA.U]) => rA.from(x))
    val fB = fA.andThen((x:B) => rB.to(x))
    doLambdaDef(fB)
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Lambda(g,x:Exp[Any],y:Block[b]) => toAtom(Lambda(f(g),f(x),f(y)))(mtype(manifest[A]),pos)
//    case Reflect(e@Apply(g,arg), u, es) => reflectMirrored(Reflect(Apply(f(g),f(arg))(e.mA,mtype(e.mB)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(x) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }

// TODO: right now were trying to hoist as much as we can out of functions.
// That might not always be appropriate. A promising strategy would be to have
// explicit 'hot' and 'cold' functions.

/*
  override def hotSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.hotSyms(e)
  }
*/

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Lambda(f, x, y) => freqHot(y)
    case _ => super.symsFreq(e)
  }

}

trait ScalaGenFunctions extends ScalaGenNested {

  val IR: FunctionsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Lambda(fun, x, y) =>
      emitValDef(sym, "{" + quote(x) + ": (" + remap(x.tp) + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + remap(y.tp))
      stream.println("}")

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
      

    case _ => super.emitNode(sym, rhs)
  }

}
