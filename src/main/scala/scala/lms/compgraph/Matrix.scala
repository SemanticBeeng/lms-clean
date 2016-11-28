/*package scala.lms
package compgraph

import common._

trait MatrixGraphs extends DerivableGraphs {


  type Data <: MatrixOps[Data]

  trait MatrixNode extends DerivableNode

  case object AddNode extends MatrixNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) + input(1)

    def d(input: Input) =
      List(one, one)    
  }

  case object MinusNode extends MatrixNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) - input(1)

    def d(input: Input) =
      List(one, zero-one)
    
  }


  case object TimeNode extends MatrixNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) * input(1)

    def d(input: Input) = {
      List(input(1), input(0))
    }
  }


  case class ConstantNode(c: Data) extends MatrixNode {
    val inputSize = 0
    def op(input: Input) =
      c

    def d(input: Input) =
      List()


    }
  
}


trait MatrixGraph extends MatrixGraphs  {

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


trait MatrixGraphExp extends MatrixGraph with Base {
  self: Rich with Matrixs =>

  type Data = Matrix[Int]

  lazy val zero: Int = 0
  lazy val one: Int = 1

  def app(a: Data) = {
    //simple(List(a, b))
//    lift(funCG.backpropagate(List(a, a, a)))
//    funCG(List(a, a, a), true)
//    add(a, a)
  }
  
  
}



//Not using staging
object MatrixGraphInt extends MatrixGraph with Matrixs with RichOptImpl {

  import Numeric._

  type Data = Matrix[IntNum]

  val zero = IntNum(0)
  val one = IntNum(1)


}

 */
