package scala.lms
package common

import internal._

import java.io.PrintWriter

import scala.reflect.SourceContext

trait Functions extends Base {

  implicit def fun[A:Rep, B:Rep](f: A => B): Lambda[A,B]

  type Lambda[A,B] <: A => B

  implicit def lambdaRep[A:Rep, B:Rep]: Rep[Lambda[A,B]]

}

trait FunctionsExp extends Functions with EffectExp {

  case class LambdaDef[A, B](x:Exp[A], b:Block[B]) extends Def[A => B]
  case class Apply[A,B](b:Exp[A => B], x:Exp[A]) extends Def[B]
  
  case class Lambda[A:Rep, B:Rep](f: A => B) extends (A => B) {

    val rA:Rep[A] = rep[A]
    val rB:Rep[B] = rep[B]

    lazy val lambda = doLambdaDef(f)(rA, rB)

    def apply(arg:A):B = {
      implicit val mf = rB.m
      val apply = Apply(lambda, rA.to(arg))
      rB.from(apply)
    }
  }
    

  implicit def lambdaRep[A:Rep,B:Rep]: Rep[Lambda[A,B]] = new Rep[Lambda[A,B]] {
    val rA = rep[A]
    val rB = rep[B]    
    implicit val mf = rA.m
    implicit val mf2 = rB.m
    val iA:rA.type = rA
    val iB:rB.type = rB
    type Internal = rA.Internal => rB.Internal

    def from(e:Exp[rA.Internal => rB.Internal]) = new Lambda[A,B]((x:A) => ???){
      override val rA:iA.type = iA
      override val rB:iB.type = iB
      override lazy val lambda = ???
      override def apply(arg:A):B = {
        rB.from(toAtom(Apply(e, rA.to(arg))))
      }
    }

    def to(l:Lambda[A,B]) = l.lambda.asInstanceOf[Exp[Internal]]
    def m = manifest[Internal];
  }


  def doLambdaDef[A, B](fun: A => B)(rA: Rep[A], rB:Rep[B]) = {

    implicit val mf = rA.m
    implicit val mf2 = rB.m    
    val x: Exp[rA.Internal] = unboxedFresh[rA.Internal]

    val fA = fun.compose((x:Exp[rA.Internal]) => rA.from(x))
    val fB: Exp[rA.Internal] => Exp[rB.Internal] = fA.andThen((x:B) => rB.to(x))
    val b: Block[rB.Internal] = reifyEffects(fB(x))
    toAtom(LambdaDef(x, b))
  }

  def unboxedFresh[A:Manifest] : Exp[A] = fresh[A]

  implicit def fun[A:Rep,B:Rep](f: A => B):Lambda[A,B]  = 
      Lambda(f)


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@LambdaDef(x:Exp[Any],y:Block[b]) => toAtom(LambdaDef(f(x), f(y)))(mtype(manifest[A]),pos)
//    case Reflect(e@Apply(g,arg), u, es) => reflectMirrored(Reflect(Apply(f(g),f(arg))(e.mA,mtype(e.mB)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  override def syms(e: Any): List[Sym[Any]] = e match {
    case LambdaDef(x, y) => syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case LambdaDef(x, y) => syms(x) ::: effectSyms(y)
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
    case LambdaDef(_, y) => freqHot(y)    
    case _ => super.symsFreq(e)
  }

}

trait ScalaGenFunctions extends ScalaGenNested {

  val IR: FunctionsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

    case e@LambdaDef(x, y) =>
      emitValDef(sym, "{" + quote(x) + ": (" + remap(x.tp) + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + remap(y.tp))
      stream.println("}")

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")


    case _ => super.emitNode(sym, rhs)
  }

}
