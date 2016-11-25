package scala.lms
package compgraph

import common._


trait ArithNodes extends Nodes {
  self: Rich =>

  override type Data =  Int
  
  case object AddNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      input(0) + input(1)
  }

  case object MinusNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      input(0) - input(1)
  }

  case object ModNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      input(0) % input(1)
  }

  case object MultNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      input(0) * input(1)
  }

  case object DivNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      input(0) / input(1)
  }
  
  case object MaxNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      ???
//      input(0) input(1))
  }

  case object MinNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      ???
//      min(input(0), input(1))
  }

  case object NegNode extends Node {
    val inputSize = 1
    def op(input: Input) =
      0 - input(0)
  }
  
}


trait ArithGraph extends Graph with ArithNodes  {

  self: Rich =>

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

/*
//Using staging
trait ArithGraphExp extends ArithGraph with Base {
  self: Rich =>

  type Data = Int

  def add(a: Int, b:Int) = a + b

  def mult(a: Int, b: Int) = a*b
  def div(a:Int, b:Int) = a/b
  def mod(a:Int, b:Int) = a%b  

  //TODO  
  def max(a:Int, b:Int) = ???
  def min(a:Int, b:Int) = ???

  def neg(a:Int) = 0 - a
  

}
*/

/*
//Not using staging
object ArithGraphInt extends ArithGraph {

  type Data = Int

  def add(a:Int, b:Int) = a + b
  def mult(a: Int, b: Int) = a*b
  def div(a:Int, b:Int) = a/b
  def mod(a:Int, b:Int) = a%b  

  def max(a:Int, b:Int) = a.max(b)
  def min(a:Int, b:Int) = a.min(b)

  def neg(a:Int) = -a
  

}

*/
