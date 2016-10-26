package scala.lms

import internal._


import scala.annotation.implicitNotFound


trait Base {
  // preliminaries
  @implicitNotFound("${T} is not a DSL type")
  type Exp[T]

  @implicitNotFound("${A} cannot be implicitly lifted to ${B}")
  type Lift[A,B]

  implicit def identLift[T:Rep]: Lift[T,T]
  implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U

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

trait BaseExp extends Base with Expressions {

  trait Lift[A,B] {
    def to(x:A):B
  }

  implicit def identLift[T:Rep]: Lift[T,T] = new Lift[T,T] { def to(x:T) = x }
  implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U = e.to(x)

  def typ[T:Rep] = implicitly[Rep[T]]

}

trait ScalaGenBase extends ScalaCodegen
trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase
trait ScalaGenFat extends ScalaFatCodegen with ScalaGenBase
