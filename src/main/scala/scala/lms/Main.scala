package scala.lms

import common._
import internal._
import java.io.{File, PrintWriter}

object Main extends App with ScalaGenNested with ScalaGenInts{

  val IR = new IntsImpl with Effects {}

  import IR._

  def run(z: Int) = {
    val x: Int = 5
    val y = x + 3
    val z = x*x+x-x
    z
  }

  //Only print the generated code in the console for now for debug purposes
  stream = new PrintWriter(System.out)
  emitBlock(reifyBlock(run(8)))
  stream.flush()

}
