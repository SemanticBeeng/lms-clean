package scala.lms

import common._
import util._
import internal._
import java.io.{File, PrintWriter}

object Main extends App with ScalaGenRich {

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
  val f = run
  implicit val mU = intTyp.m
  implicit val m = manifest[intTyp.U => intTyp.U]
  emitBlock(reifyBlock(f))
  stream.flush()

}
