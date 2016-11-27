package scala.lms
package common

import java.io.PrintWriter
import scala.lms.util.OverloadHack
import scala.reflect.SourceContext


trait Equals extends Base  {
  self: Booleans =>
  //TODO: Ruben: re-include var
  def __equal[A:Rep, B:Rep](a:A, b:B)(implicit pos: SourceContext) : Boolean
  def __notequal[A:Rep, B:Rep](a: A, b: B)(implicit pos: SourceContext) : Boolean

}

trait EqualsExp extends Equals with BaseExp {
  self: Booleans =>

  case class Equal[A,B](a: Exp[A], b: Exp[B]) extends Def[scala.Boolean] 
  case class NotEqual[A,B](a: Exp[A], b: Exp[B]) extends Def[scala.Boolean] 


  def equal[A,B](a: Exp[A], b: Exp[B])(implicit pos: SourceContext) : Exp[scala.Boolean]
  def notequal[A,B](a: Exp[A], b: Exp[B])(implicit pos: SourceContext) : Exp[scala.Boolean]


  def __equal[A:Rep,B:Rep](a: A, b: B)(implicit pos: SourceContext): Boolean = {
    val repA = rep[A]
    val repB = rep[B]    
    booleanRep.from(equal(repA.to(a), repB.to(b)))
  }

  def __notequal[A:Rep,B:Rep](a: A, b: B)(implicit pos: SourceContext): Boolean = {
    val repA = rep[A]
    val repB = rep[B]        
    booleanRep.from(notequal(repA.to(a), repB.to(b)))
  }

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case e@Equal(a, b) => Equal(f(a),f(b))
    case e@NotEqual(a, b) => NotEqual(f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Equal(a, b) => equal(f(a),f(b))(pos)
    case e@NotEqual(a, b) => notequal(f(a),f(b))(pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait EqualsImpl extends EqualsExp {
  self: Booleans =>

  def equal[A,B](a: Exp[A], b: Exp[B])(implicit pos: SourceContext): Exp[scala.Boolean] = Equal(a,b)
  def notequal[A,B](a: Exp[A], b: Exp[B])(implicit pos: SourceContext): Exp[scala.Boolean] = NotEqual(a,b)

}


trait EqualsOptImpl extends EqualsImpl {
  self: Booleans =>

  override def equal[A,B](a: Exp[A], b: Exp[B])(implicit pos: SourceContext): Exp[scala.Boolean] = if (a.equals(b)) Const(true) else (a,b) match {
    case (Const(a),Const(b)) => Const(a.equals(b))
    case _ => super.equal(a,b)
  }

  override def notequal[A,B](a: Exp[A], b: Exp[B])(implicit pos: SourceContext): Exp[scala.Boolean] = if (a.equals(b)) Const(false) else (a,b) match {
    case (Const(a),Const(b)) => Const(!equals(a,b))
    case _ => super.notequal(a,b)
  }
}



trait ScalaGenEquals extends ScalaGenBase {
  val IR: EqualsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, src"$a == $b")
    case NotEqual(a,b) =>  emitValDef(sym, src"$a != $b")
    case _ => super.emitNode(sym, rhs)
  }
}




