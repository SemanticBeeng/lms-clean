package scala.lms

import common._
import internal._

trait Arith extends Graph with Nodes  {

  def simple = {
    val l = List(GraphNode("ADD", new AddNode {}, List("IN1", "IN2")))
    newGraph(l, 2, "ADD")
  }

  def fun = {
    val l = List(
      GraphNode("ADD", new AddNode {}, List("IN1", "IN2")),
      GraphNode("ADD2", new AddNode {}, List("ADD", "ADD"))        
    )
    newGraph(l, 2, "ADD2")
  }
  
  def main(a: Data, b:Data) = {
    //simple(List(a, b))
    fun(List(a, b))
  }
  

}

//Using staging
trait ArithExp extends Arith {
  val IR: Rich

  import IR._

  type Data = Int
  def add(a:Int, b:Int) = a + b

}

//Not using staging
object ArithInt extends Arith {

  type Data = Int
  def add(a:Int, b:Int) = a + b
}




trait Nodes {
  type Data
  type Input = List[Data]
  type Output = Data

  def add(a:Data, b:Data): Data

  sealed trait Node {
    def inputSize: Int
    def outputSize: Int
    def out(input: Input) = {
      require(input.length == inputSize)
      val r = f(input)
      List.fill(outputSize)(r)
    }

    def f(input: Input): Output

  }

  trait OutputNode extends Node {
    val inputSize = 1
    val outputSize = 1
    def f(input: Input) = input.head
  }

  trait InputNode extends Node {
    val inputSize = 0
    val outputSize = 1    
    def f(input: Input) = input.head
  }
  
  trait AddNode extends Node {
    val inputSize = 2
    val outputSize = 1            
    def f(input: Input) = add(input(0), input(1))
  }


}

trait Graph {
  nodes: Nodes =>
  import nodes._

  type R = (Node, List[String])
  case class GraphNode(name: String, n: Node, inputs: List[String])

  def newGraph(l: List[GraphNode], inputSize:Int, outInput: String) = {
    var m: Map[String, R] = Map(("OUT", (new OutputNode {}, List(outInput))))
    (1 to inputSize).foreach(x => m += (("IN"+x, (new InputNode {}, List()))))
    l.foreach(x => m += ((x.name, (x.n, x.inputs))))
    Graph(m)
  }


  case class Graph(nodes: Map[String, R] = Map()) {
    def apply(input: List[Data]) = {
      var datas = input.zipWithIndex map { case (data, ind) => ("IN"+(ind+1), data) } toMap
      def rec(s: String):Data = {
        val (n, inp) = nodes(s)
        if (datas.contains(s))
          datas(s)
        else {
          val r = n.f(inp.map(rec))
          datas += ((s, r))
          r
        }
      }
      rec("OUT")

    }
  }
}
