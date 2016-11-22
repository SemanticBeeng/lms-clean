package scala.lms
package common

import internal._

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}


trait IfThenElse extends Base{
  this: Booleans =>

  def __ifThenElse[A:Rep](cond: Boolean, thenp: => A, elsep: => A)(implicit pos: SourceContext): A

  /*
  // HACK -- bug in scala-virtualized
  override def __ifThenElse[T](cond: => scala.Boolean, thenp: => T, elsep: => T) = cond match {
    case true => thenp
    case false => elsep
  }
   */
}


trait IfThenElsePureExp extends BaseExp with IfThenElse  {
  this: Booleans =>

  case class IfThenElse[T](cond: Exp[scala.Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]

  def __ifThenElse[A:Rep](cond: Boolean, thenp: => A, elsep: => A)(implicit pos: SourceContext): A = {
    val tp = typ[A]
    implicit val mf = tp.m
    val thenpC = tp.to(thenp)
    val elsepC = tp.to(elsep)
    val condBool: Exp[scala.Boolean] = booleanTyp.to(cond)
    tp.from(IfThenElse(condBool, thenpC, elsepC))
  }
}



trait IfThenElseExp extends IfThenElse with EffectExp {
  this: Booleans =>

  abstract class AbstractIfThenElse[T] extends Def[T] {
    val cond: Exp[scala.Boolean]
    val thenp: Block[T]
    val elsep: Block[T]
  }
  
  case class IfThenElse[T](cond: Exp[scala.Boolean], thenp: Block[T], elsep: Block[T]) extends AbstractIfThenElse[T]

/*  override def __ifThenElse[A:Rep,B](cond: Boolean, thenp: A, elsep: B)(implicit pos: SourceContext,  mB: Lift[B,A]): A = {
    val tp = typ[A]
    implicit val mf = tp.m    
    val a = reifyEffectsHere(tp.to(thenp))
    val b = reifyEffectsHere(tp.to(mB.to(elsep)))
    tp.from(ifThenElse(cond,a,b))
  }
 */
  def ifThenElse[T:Manifest](cond: Boolean, thenp: Block[T], elsep: Block[T])(implicit pos: SourceContext) = {
    val ae = summarizeEffects(thenp)
    val be = summarizeEffects(elsep)
    
    // TODO: make a decision whether we should call reflect or reflectInternal.
    // the former will look for any read mutable effects in addition to the passed
    // summary whereas reflectInternal will take ae orElse be literally.
    // the case where this comes up is if (c) a else b, with a or b mutable.
    // (see TestMutation, for now sticking to old behavior)
    
    ////reflectEffect(IfThenElse(cond,thenp,elsep), ae orElse be)
    val condBool: Exp[scala.Boolean] = booleanTyp.to(cond).asInstanceOf[Exp[scala.Boolean]]    
    reflectEffectInternal(IfThenElse(condBool,thenp,elsep), ae orElse be)
  }
  
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case IfThenElse(c,a,b) => IfThenElse(f(c),f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    e match {
      /*
    case Reflect(IfThenElse(c,a,b), u, es) => 
      if (f.hasContext)
        __ifThenElse(f(c), f.reflectBlock(a), f.reflectBlock(b))
      else
        reflectMirrored(Reflect(IfThenElse(f(c),f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case IfThenElse(c,a,b) => 
      if (f.hasContext)
        __ifThenElse(f(c), f.reflectBlock(a), f.reflectBlock(b))
      else
        IfThenElse(f(c),f(a),f(b)) // FIXME: should apply pattern rewrites (ie call smart constructor)
       */
    case _ => super.mirror(e,f)
    }
  }
/*
  override def mirror[A:Typ](e: Def[A], f: Transformer): Exp[A] = e match {
    case Reflect(IfThenElse(c,a,b), u, es) => mirror(IfThenElse(c,a,b)) // discard reflect
    case IfThenElse(c,a,b) => ifThenElse(f(c),f(a),f(b)) // f.apply[A](a: Block[A]): Exp[A] mirrors the block into the current context
    case _ => super.mirror(e,f)
  }  
*/



  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => syms(a):::syms(b)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c,a,b) => Nil // could return a,b but implied by aliasSyms
    case _ => super.copySyms(e)
  }


  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case IfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)
    case _ => super.symsFreq(e)
  }

/*
  override def coldSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) => syms(t) ++ syms(e)
    case _ => super.coldSyms(e)
  }
*/

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }

}

trait ScalaGenIfThenElsePure extends ScalaGenNested {
  val IR: IfThenElsePureExp with Effects
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IfThenElse(c,a,b) =>
      val blockA = reifyBlock(a)
      val blockB = reifyBlock(b)
      stream.println("val " + quote(sym) + " = if (" + quote(c) + ") {")
      emitBlock(blockA)
      stream.println(quote(getBlockResult(blockA)))
      stream.println("} else {")
      emitBlock(blockB)
      stream.println(quote(getBlockResult(blockB)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}
