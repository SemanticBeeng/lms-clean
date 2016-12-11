package scala.lms
package compgraph

import collection.mutable.Queue

import common._

trait DerivableGraphs extends Graphs {
  
  def zero: Data
  def one: Data

  type Data <: AddTimeAble[Data]

  type Derivatives = List[Data]
  type N = DerivableNode
  type G = DerivableGraph

  trait DerivableNode extends Node {

    def derivative(input: Input):Derivatives = {
      require(input.length == inputSize)
      d(input)
    }

    protected def d(input: Input): Derivatives
    
  }

  trait OutputNode extends DerivableNode with super.OutputNode{
    protected def d(input: Input): Derivatives = 
      List(one)

  }
  def OutputNode():N = new OutputNode {}

  trait InputNode extends DerivableNode with super.InputNode{
    protected def d(input: Input): Derivatives = {
      throw new Exception("Is not a processing node")
    }
  }

  def InputNode():N= new InputNode {}
  

  trait DerivableGraph extends Graph {

    //Credits: https://gist.github.com/ThiporKong/4399695
    def tsort[A](edges: Traversable[(A, A)]): Iterable[A] = {
//      @tailrec
      def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
        val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
        if (noPreds.isEmpty) {
          if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
        } else {
          val found = noPreds.map { _._1 }
          tsort(hasPreds.mapValues { _ -- found }, done ++ found)
        }
      }

      val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
        acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
      }
      tsort(toPred, Seq())
    }

    def backpropagate(input: IndexedSeq[Data], dbg:Boolean = false): List[Data] = {

      var derivatives = Map[String, Data](("OUT", one)).withDefaultValue(zero)

      val datas = forward(input, dbg)._2

      val q = new Queue[String]()

      q.enqueue("OUT")

      var s: Set[(String, String)] = Set()

      while(!q.isEmpty) {
        val nStr = q.dequeue()
        val (node, inputs) = nodes(nStr)
        inputs.foreach(q.enqueue(_))
        inputs.foreach(x => s += ((nStr,x)))
      }

      tsort(s.toList).foreach(q.enqueue(_))

      while(!q.isEmpty) {
        val nStr = q.dequeue()
        if (!nStr.contains("IN")) {
          val (node, inputs) = nodes(nStr)
          val deriv = node.derivative(inputs.map(datas))
          val thisD = derivatives(nStr)
          deriv.zip(inputs).foreach(x =>
            derivatives += ((x._2, derivatives(x._2) + x._1*thisD))
          )
        }
      }
      (1 to inputSize).map("IN"+_).map(derivatives).toList
    }
  }
  
  def Graph(m: Map[String, R], iS: Int) = new DerivableGraph {
    def nodes = m
    def inputSize = iS 
  }

}
