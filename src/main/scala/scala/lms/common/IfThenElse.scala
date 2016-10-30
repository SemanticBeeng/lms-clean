package scala.lms
package common

import internal._

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}


trait IfThenElse extends Base{
  this: Booleans =>

  def __ifThenElse[A:Rep, B](cond: Boolean, thenp: => A, elsep: => B)(implicit pos: SourceContext, mB: Lift[B,A]): A

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

  def __ifThenElse[A:Rep,B](cond: Boolean, thenp: => A, elsep: => B)(implicit pos: SourceContext,  mB: Lift[B,A]): A = {
    val tp = typ[A]
    implicit val mf = tp.m
    val thenpC = tp.to(thenp)
    val elsepC = tp.to(mB.to(elsep))
    val condBool: Exp[scala.Boolean] = booleanTyp.to(cond).asInstanceOf[Exp[scala.Boolean]]
    tp.from(IfThenElse(condBool, thenpC, elsepC))
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
