package scala.lms

import common._
import internal._
import java.io.{File, PrintWriter}

object Main extends App with ScalaGenEffect with ScalaGenInts{

  val IR = new IntsImpl with Effects {}

  import IR._

  def run(z: Int) = {
    val x: Int = 5
    val y = x + 3
    y
  }

  stream = new PrintWriter(System.out)
  emitBlock(reifyBlock(run(8)))
  stream.flush()

}
