package scala.lms
package compgraph

import common._

trait ArithGraphs extends DerivableGraphs {


  trait ArithNode extends DerivableNode

  case object AddNode extends ArithNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) + input(1)

    def d(input: Input) =
      List(one, one)    
  }

  case object MinusNode extends ArithNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) - input(1)

    def d(input: Input) =
      List(one, zero-one)
    
  }

  case object ModNode extends ArithNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) % input(1)

    def d(input: Input) =
      ???

  }

  case object TimeNode extends ArithNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) * input(1)

    def d(input: Input) = {
      List(input(1), input(0))
    }
  }

  case object DivNode extends ArithNode {
    val inputSize = 2
    def op(input: Input) =      
      input(0) / input(1)

    def d(input: Input) =
      List(one/input(1), (zero-input(0))/input(1)/input(1))    
  }
  
  case object MaxNode extends ArithNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) max input(1)

    def d(input: Input) =
      ???
    
  }

  case object MinNode extends ArithNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) min input(1)

    def d(input: Input) =
      ???
    
  }

  case class ConstantNode(c: Data) extends ArithNode {
    val inputSize = 0
    def op(input: Input) =
      c

    def d(input: Input) =
      List()


    }
  
}


trait ArithGraph extends ArithGraphs  {

  def simpleCG = {
    val l = List(GraphNode("ADD", AddNode, List("IN1", "IN2")))
    newGraph(l, 2, "ADD")
  }

  def funCG = {
    val l = List(
      GraphNode("ADD", AddNode, List("IN1", "IN2")),
      GraphNode("ADD2", TimeNode, List("ADD", "ADD")),
      GraphNode("ADD3", AddNode, List("ADD2", "IN3"))              
    )
    newGraph(l, 3, "ADD3")
  }
  
  

}


trait ArithGraphExp extends ArithGraph with Base {
  self: Rich =>

  type Data = Int

  lazy val zero: Int = 0
  lazy val one: Int = 1

  def app(a: Data) = {
    //simple(List(a, b))
    lift(funCG.backpropagate(List(a, a, a)))
//    funCG(List(a, a, a), true)
//    add(a, a)
  }
  
  
}



//Not using staging
object ArithGraphInt extends ArithGraph {

  type Data = IntNum

  val zero = IntNum(0)
  val one = IntNum(1)

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

