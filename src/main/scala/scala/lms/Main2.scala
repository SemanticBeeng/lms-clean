/*package test

import scala.annotation.implicitNotFound

trait Base {
  // preliminaries
  @implicitNotFound("${T} is not a DSL type")
  type Exp[+T]

  trait Lift[A,B] {
    def lift(x:A):B
  }

  implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U = e.lift(x)


}

trait Rep[T] {
  type Internal
  def m: Manifest[Internal]
}


trait BaseExp {
  abstract class Exp[+T]
  abstract class Def[+T] extends Exp[T]
  case class Const[T](x: T) extends Exp[T]

  trait Frontend[A, B] extends Exp[B] {
    type Internal = B
    val e = this.asInstanceOf[A]
  }

  implicit def toRep[A,B:Manifest](f: Frontend[A,B]):Rep[A] = new Rep[A] {
    type Internal = B
    def m = manifest[B]
  }
}



trait Ints extends Base {
  

  type Int

  implicit def intLift: Lift[scala.Int, Int]

}

trait IntsImpl extends Ints with BaseExp {

  class Int extends Frontend[Int, scala.Int] {
    def +(x: Int) = int(plus(e, x.e))
  }

  def int(x: Exp[scala.Int]) = x.asInstanceOf[Int]

  implicit def intLift = new Lift[scala.Int, Int] { def lift(x: scala.Int) = Const(x).asInstanceOf[Int] }  


  case class Plus(x: Exp[scala.Int], y: Exp[scala.Int]) extends Def[scala.Int]

  def plus(x:Exp[scala.Int], y:Exp[scala.Int]): Exp[scala.Int] = Plus(x, y)
y1

}

object Main extends Base with IntsImpl {

  val a: Int = 3
  val b: Int = a + 3

  def f[A:Rep](x: A) = println(x)
  f(a)
//  f(3)





}
 */
