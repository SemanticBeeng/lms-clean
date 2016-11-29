package scala.lms


import common._
import util._
import internal._
import java.io.{File, PrintWriter}
import compgraph._

trait Prog extends Rich with MatrixGraph {

  def g[A:Rep](l:Int => Int, x:Int) =
    l(x)*2

  val f = (arg:Int) =>  {
    val stagedApp = fun(app)
    stagedApp(arg)
  }

  val smallProg = (p:Int) => {
    val x: Int = 4
    val y: Int = 3 + x
    val z: Int = x*p+x-x
    val b: Boolean = true
    val c: scala.Boolean = true
    b

  }
  
}


object Main extends App with RichOptImpl with Compile with Prog{


  val exec = scala.List(
//    ("Prog", smallProg, 1)
    ("Staged execution", f, 3)
  )

  exec.foreach { case (title, f, arg) => 
    println(title+":")
    val staged = compile(f)
    val r = staged(arg)
    println(r)
  }

/*
 println("Non staged execution:")
  val r3 = ArithGraphInt.app(5)
  println(r3)
 */

}

trait Compile extends ScalaCompile {
  self: RichExp =>

  dumpGeneratedCode = true
  val codegen =  new ScalaGenRich { val IR:self.type = self}

  implicit def comp[A,B](f: A => B)(implicit repA: Rep[A], repB:Rep[B]): Exp[repA.Internal] => Exp[repB.Internal] = {
    (x:Exp[repA.Internal]) => repB.to(f(repA.from(x)))
  }

}

