package scala.lms
package common

import internal._

import java.io.PrintWriter


import scala.reflect.SourceContext

trait Functions extends Base {

  implicit def fun[A, B](fun: A => B)(implicit tA:Rep[A], tB:Rep[B]): Exp[tA.U => tB.U]

}

trait FunctionsExp extends Functions with BaseExp with Effects {

  case class Lambda[A, B](f: Exp[A] => Exp[B], x: Exp[A], y: Block[B]) extends Def[A => B]

  def unboxedFresh[A:Manifest] : Exp[A] = fresh[A]
  def unbox[A:Manifest](x : Exp[A])(implicit pos: SourceContext) : Exp[A] = x

  def doLambdaDef[A:Manifest, B:Manifest](f: Exp[A] => Exp[B]) : Def[A => B] = {
    val x = unboxedFresh[A]
    val y = reifyEffects(f(x)) // unfold completely at the definition site.
    Lambda(f, x, y)
  }

  implicit def fun[A,B](fun: A => B)(implicit rA:Rep[A], rB:Rep[B]) = {
    implicit val mA = rA.m
    implicit val mB = rB.m
    val fA = fun.compose((x:Exp[rA.U]) => rA.from(x))
    val fB = fA.andThen((x:B) => rB.to(x))
    val lf: Def[rA.U => rB.U] = doLambdaDef(fB)
    toAtom(lf)
  }
/*
  implicit def funTyp[A:Rep, B:Rep](fun: A => B): Rep[A => B] = new Rep[A=>B]{
    val tpA = typ[A]
    val tpB = typ[B]
    type U = tpA.U => tpB.U
    implicit val mA = tpA.m
    implicit val mB = tpB.m
    implicit val m = manifest[U]
    def from(x: Exp[U]) = fun
    def to(x: A => B) = doLambdaDef(fun.compose((x:Exp[tpA.U]) => tpA.from(x)).andThen((x:B) => tpB.to(x)))
  }
 */
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

/*    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
 */

    case _ => super.emitNode(sym, rhs)
  }
}
