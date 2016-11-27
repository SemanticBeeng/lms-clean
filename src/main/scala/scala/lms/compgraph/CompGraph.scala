package scala.lms
package compgraph

import collection.mutable.Queue


trait Graph {
  nodes: Nodes =>
  import nodes._

  type R = (Node, List[String])
  type G <: Graph
  case class GraphNode(name: String, n: Node, inputs: List[String])

  def newGraph(l: List[GraphNode], inputSize:Int, outInput: String): G = {
    var m: Map[String, R] = Map(("OUT", (OutputNode(), List(outInput))))
    (1 to inputSize).foreach(x => m += (("IN"+x, (InputNode(), List()))))
    l.foreach(x => m += ((x.name, (x.n, x.inputs))))
    Graph(m, inputSize)
  }

  def Graph(m: Map[String, R], inputSize: Int): G

  trait Graph {

    def inputSize: Int

    def nodes: Map[String, R]

    def debug(str:String, dbg:Boolean) =
      if (dbg)
        println("[debug] " +str)

    def apply(input: List[Data], dbg:Boolean = false) =
      forward(input, dbg)._1

    def forward(input: List[Data], dbg:Boolean = false) = {

      //init datas with provided input as input Node
      var datas: Map[String, Data] =
        input.zipWithIndex map { case (data, ind) => ("IN"+(ind+1), data) } toMap

      debug("datas init " + datas.toString, dbg)

      def output(s: String):Data = {

        debug("rec call " + s, dbg)
        val (n, inp) = nodes(s)
        if (datas.contains(s)) {
          debug("cache hit " + datas, dbg)
          datas(s)
        }
        else {
          val l = inp.map(output)
          val r = n.output(l)
          debug("op " + n.getClass.getName + " input:" + inp + "l: " + l + " v:" + r , dbg)          
          datas += ((s, r))
          r
        }
      }

      (output("OUT"), datas)

    }
  }
  
}

trait NonDerivableGraph extends Graph {
  self: Nodes =>

  type G = Graph

  def Graph(m: Map[String, R], iS: Int) = new Graph {
    def nodes = m
    def inputSize = iS
  }
  

}
trait DerivableGraph extends Graph {
  self: DerivableNodes =>

  type G = Graph


  trait Graph extends super.Graph {
    def backpropagate(input: List[Data], dbg:Boolean = false): List[Data] = {
      val datas = forward(input, dbg)
      val q = new Queue[String]()
      q.enqueue("OUT")
      while(!q.isEmpty) {
        q.dequeue()
      }
      List(zero)
    }
  }
  
  def Graph(m: Map[String, R], iS: Int):Graph = new Graph {
    def nodes = m
    def inputSize = iS 
  }

  override def newGraph(l: List[GraphNode], inputSize:Int, outInput: String): Graph =
    super.newGraph(l, inputSize, outInput)
}
