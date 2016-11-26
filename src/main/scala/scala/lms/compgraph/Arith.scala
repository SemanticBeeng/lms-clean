package scala.lms
package compgraph

import common._


trait ArithNodes extends Nodes {
  type Data <: Num[Data]
  
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
      input(0) max input(1)
  }

  case object MinNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      input(0) min input(1)      
  }

  case class ConstantNode(c: Data) extends Node {
    val inputSize = 0
    def op(input: Input) =
      c

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
  
  

}


trait ArithGraphExp extends ArithGraph with Base {
  self: Rich =>

  type Data = Int

  def app(a: Data) = {
    //simple(List(a, b))
    funCG(List(a, a, a), true)
//    add(a, a)
  }
  
  
}



//Not using staging
object ArithGraphInt extends ArithGraph {

  type Data = IntNum

  case class BooleanO(x: Boolean) extends BooleanOps[BooleanO] {
    type A = BooleanO
    def &&(y: => A): A = BooleanO(x && y.x)
    def ||(y: => A): A = BooleanO(x || y.x)
    def unary_! : A = BooleanO(!x)
  }

  case class IntNum(x: Int) extends Num[IntNum] {
    type A = IntNum
    override type B = BooleanO
    def +(y: A): A = IntNum(x + y.x)
    def -(y: A): A = IntNum(x - y.x)
    def *(y: A): A = IntNum(x * y.x)
    def /(y: A): A = IntNum(x / y.x)
    def %(y: A): A = IntNum(x % y.x)
    def ===(y: A): B = BooleanO(x == y.x)
    def >(y: A): B = BooleanO(x > y.x)
    def <(y: A): B = BooleanO(x < y.x)
    def min(y: A): A = IntNum(x.min(y.x))
    def max(y: A): A = IntNum(x.max(y.x))    
  }

  def app(b: Int) = {
    val a = IntNum(b)
    //simple(List(a, b))
    funCG(List(a, a, a), true)
//    add(a, a)
  }
  
}


