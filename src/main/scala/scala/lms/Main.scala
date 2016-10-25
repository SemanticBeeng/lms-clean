package scala.lms

import internal._

object Main extends App {

  val IR: DSL = new Impl {}

  import IR._

  def run(z: Int) = {
    val x: Int = 5
    val y = x + 3
    y
  }


  println(run(8))

}
