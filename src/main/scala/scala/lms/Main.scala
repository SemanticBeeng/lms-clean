package scala.lms

import internal._

object Main extends App with Impl{

  def run(z: Int) = {
    val x: Int = 5
    val y = x + 3
    y
  }


  println(run(8))

}
