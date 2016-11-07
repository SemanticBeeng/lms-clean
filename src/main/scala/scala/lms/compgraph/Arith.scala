package scala.lms
package compgraph

import common._


trait ArithNodes extends Nodes {

  def add(a:Data, b:Data): Data

  case object AddNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      add(input(0), input(1))
  }
  
}


trait ArithGraph extends Graph with ArithNodes  {

  def simpleCG = {
    val l = List(GraphNode("ADD", AddNode, List("IN1", "IN2")))
    newGraph(l, 2, "ADD")
  }

  def funCG = {
    val l = List(
      GraphNode("ADD", AddNode, List("IN1", "IN2")),
      GraphNode("ADD2", AddNode, List("ADD", "ADD")),
      GraphNode("ADD3", AddNode, List("ADD2", "IN3"))              
    )
    newGraph(l, 3, "ADD3")
  }
  
  def app(a: Data) = {
    //simple(List(a, b))
    funCG(List(a, a, a), true)
//    add(a, a)
  }
  

}

//Using staging
trait ArithGraphExp extends ArithGraph {

  val IR: Rich
  import IR._
  type Data = Int

  def add(a:Int, b:Int) = a + b

}

//Not using staging
object ArithGraphInt extends ArithGraph {

  type Data = Int

  def add(a:Int, b:Int) = a + b

}

