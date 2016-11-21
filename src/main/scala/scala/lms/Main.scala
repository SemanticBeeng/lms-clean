package scala.lms


import common._
import util._
import internal._
import java.io.{File, PrintWriter}
import compgraph._

object Main extends App with ArithGraphExp with RichImpl with Compile {

  def f(p:Exp[scala.Int]) = {
    val stagedApp: LambdaApply[scala.Int, scala.Int]  = fun(app)
    stagedApp(p)
  }
  
  println("Staged execution:")  
  val staged = compile(f)
  val r1 = staged(5)
  println(r1)


  println("Non staged execution:")
  val r2 = ArithGraphInt.app(2)
  println(r2)
  
}

trait Compile extends ScalaCompile {
  self:RichImpl =>

  dumpGeneratedCode = true
  val codegen =  new ScalaGenRich { val IR:self.type = self}

}

trait Prog {

  val run = (p: Int) =>  {
    val x: Int = 4
    val y: Int = x + 3
    val z: Int = x*p+x-x
    val b: Boolean = true
    if (b)
      z
    else
      p
  }
  

}
