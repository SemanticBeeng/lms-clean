package scala.lms


import common._
import util._
import internal._
import java.io.{File, PrintWriter}
import compgraph._


object Main extends App with ArithGraph with RichOptImpl with Compile {

  def g[A:Rep](l:Int => Int, x:Int) =
    l(x)*2

  def f(p:Exp[scala.Int])  = {
    val arg = intTyp.from(p)
    val b: Boolean = true
    val stagedApp = if (b) fun(app) else fun(app)
    val r = stagedApp(arg)
    intTyp.to(r)
  }
  
  println("Staged execution:")  
  val staged = compile(f)
  val r1 = staged(5)
  println(r1)

/*
  println("Non staged execution:")
  val r2 = ArithGraphInt.app(2)
  println(r2)
 */
}

trait Compile extends ScalaCompile {
  self: RichExp =>

  dumpGeneratedCode = true
  val codegen =  new ScalaGenRich { val IR:self.type = self}

}

trait Prog {

  val run = (p: Int) =>  {
    val x: Int = 4
    val y: Int = 3 + x
    val z: Int = x*p+x-x
    val b: Boolean = true
    if (b)
      z
    else
      p
  }
  

}
