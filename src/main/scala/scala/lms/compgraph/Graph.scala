package scala.lms
package compgraph

import collection.mutable.Queue

trait Graphs {

  type Data
  type Input = List[Data]
  type Output = Data
  type G <: Graph 
  type N <: Node

  type R = (N, List[String])


  trait Node {

    def inputSize: Int

    def output(input: Input) = {
      require(input.length == inputSize)
      op(input)
    }

    protected def op(input: Input): Output
  
  }

  trait OutputNode extends Node {

    val inputSize = 1
    def op(input: Input) =
      input.head
  
  }

  trait InputNode extends Node {
    val inputSize = 0
    def op(input: Input) =
      throw new Exception("Is not a processing node")
  }

  def OutputNode() :N
  def InputNode(): N

  case class GraphNode(name: String, n:N, inputs:List[String])

  def newGraph(l: List[GraphNode], inputSize:Int, outInput: String):G = {
    var m: Map[String, R] = Map(("OUT", (OutputNode(), List(outInput))))
    (1 to inputSize).foreach(x => m += (("IN"+x, (InputNode(), List()))))
    l.foreach(x => m += ((x.name, (x.n, x.inputs))))
    Graph(m, inputSize)
  }

  def Graph(m: Map[String, R], iS: Int):G 

  trait Graph {

    //check if there is cycle in the graph

//    checkCycle()

    def inputSize: Int

    def nodes: Map[String, R]

    def debug(str: =>String, dbg:Boolean) =
      if (dbg)
        println("[debug] " +str)

    def apply(input: IndexedSeq[Data], dbg:Boolean = false) =
      forward(input, dbg)._1

    def checkCycle() = {
      println("check cycle")
      val nsByN = nodes.map(x => (x._1, x._2._2))
      def hasCycle(nAndNs: (String, List[String]), visited: Set[String] = Set[String]()): Boolean = {
        if (visited.contains(nAndNs._1))
          true
        else
          nAndNs._2.exists(
            n =>
            nsByN.get(n) match {
              case Some(ns) =>
                hasCycle((n, ns), visited + nAndNs._1)
              case None =>
                false
            }
          )
      }
      val cycle = nsByN.exists(hasCycle(_))
      if (cycle)
        throw new Exception("Cycle inside the graph. ABORT")
      println("cycle checked")
    }


    def forward(input: IndexedSeq[Data], dbg:Boolean = false) = {

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



trait SimpleGraphs extends Graphs{

  type N = Node
  type G = Graph

  def OutputNode():N = new OutputNode {}  
  def InputNode():N = new InputNode {}
  

  def Graph(m: Map[String, R], iS: Int):G = new Graph {
    def inputSize = iS
    def  nodes = m
  }
  

}
