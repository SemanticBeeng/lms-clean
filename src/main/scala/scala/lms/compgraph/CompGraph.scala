package scala.lms
package compgraph


trait Graph {
  nodes: Nodes =>
  import nodes._

  type R = (Node, List[String])
  case class GraphNode(name: String, n: Node, inputs: List[String])

  def newGraph(l: List[GraphNode], inputSize:Int, outInput: String) = {
    var m: Map[String, R] = Map(("OUT", (OutputNode, List(outInput))))
    (1 to inputSize).foreach(x => m += (("IN"+x, (InputNode, List()))))
    l.foreach(x => m += ((x.name, (x.n, x.inputs))))
    Graph(m)
  }


  case class Graph(nodes: Map[String, R] = Map()) {

    def debug(str:String, dbg:Boolean) =
      if (dbg)
        println("[debug] " +str)

    def apply(input: List[Data], dbg:Boolean = false) = {

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

      output("OUT")

    }
  }
  
}
