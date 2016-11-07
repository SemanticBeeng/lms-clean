package scala.lms


import common._
import util._
import internal._
import java.io.{File, PrintWriter}
import compgraph._

object Main extends App with ScalaGenRich with ArithGraphExp{

  
  val IR  = new RichImpl {}

  import IR._

  

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
  
  stream = new PrintWriter(System.out)
  implicit val mU = intTyp.m
  implicit val m = manifest[intTyp.U => intTyp.U]
//    val f = run
  val f = (p: Int) => app(p)
  emitBlock(reifyBlock(f))
  stream.flush()

  println("non staged execution")
  println(ArithGraphInt.app(2))
}

