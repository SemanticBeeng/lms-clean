package scala.lms


import common._
import util._
import internal._
import java.io.{File, PrintWriter}
import compgraph._

object Main extends App with ArithGraphExp with RichImpl with Compile {

  dumpGeneratedCode = true
  
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


  //Only print the generated code in the console for now for debug purposes
  

  implicit val mU = intTyp.m
  implicit val m = manifest[intTyp.U => intTyp.U]
  //    val f = run

  val f:Exp[scala.Int] => Exp[intTyp.U] = (p:Exp[scala.Int]) => applyOps(fun((p: Int) => app(p))).apply(p.asInstanceOf[Exp[intTyp.U]])
//  codegen.stream = new PrintWriter(System.out)  
//  val b = codegen.reifyBlock(f)
//  codegen.emitBlock(b)
//  codegen.stream.flush()

  val c = compile(f)
  println(c(5))


  println("non staged execution")
  println(ArithGraphInt.app(2))
  
}

trait Compile extends ScalaCompile {
  self:RichImpl =>
  
  val codegen =  new ScalaGenRich { val IR:self.type = self}

}
