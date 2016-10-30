package scala.lms

import common._
import util._
import internal._
import java.io.{File, PrintWriter}

object Main extends App with ScalaGenRich {

  val IR  = new RichImpl {}

  import IR._

  val run = (p: Int) =>  {
    val x: Int = p
    val y: Int = x + 3
    val z: Int = x*x+x-x
    val b: Boolean = true
    if (b)
      z
    else
      y

  }

  //Only print the generated code in the console for now for debug purposes
  stream = new PrintWriter(System.out)
  val k: Int  = 8
  val f = run
  println(f)
  emitBlock(reifyBlock(f))
  stream.flush()

}
