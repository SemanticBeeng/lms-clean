package scala.lms
package compgraph

import common._
import Numeric._

trait Benchmark {
  self: ArithGraph =>

  val prng = new scala.util.Random(123)
  def intToData: Int => Data

  val nodes = List(
    () => AddNode,
    () => TimeNode,
    () => MinusNode,
    //    DivNode,
//    ModNode,
    () => MaxNode,
    () => MinNode,
    () => ConstantNode(intToData(prng.nextInt(5)))
  )

  def graph(size: Int, input:Int) = {
    var nodeMap = (1 to input).map(i => ("IN"+i, 0)).toMap
    var gn: List[GraphNode] = List()

    def randomNode() = {
      val min = nodeMap.values.min
      val l = nodeMap.filter(_._2 == min).keys.toList
      val r = l(prng.nextInt(l.length))
      nodeMap += ((r, min+1))
      r
    }

    for (i <- (1 to size)) {
      val name = "N"+i
      val n = nodes(prng.nextInt(nodes.length))()
      val inputs = (1 to n.inputSize).map(j => randomNode()).toList
      nodeMap += ((name, 0))
      gn ::= GraphNode(name, n, inputs)       
    }
    newGraph(gn, input, "N"+size)
  }

  val inputSize = 100
  val graphSize = 2000
  val nbGraphs = 100
  val epoch = 1

  lazy val gs = (1 to nbGraphs).map(x => graph(graphSize, inputSize))


}

object BenchmarkInt extends Benchmark with ArithGraphInt with App {

  def intToData = IntNum
  def nextData() = intToData(prng.nextInt(10))
  def nextInput() = (1 to inputSize).map(x => nextData()).toIndexedSeq
  
  def benchmark() = {
    var delta:Long = 0
    for (g <- gs) {
      println("Graph evaluation")
      val ni = nextInput()
      val startTime = System.nanoTime()
      val r = g.apply(ni)
      val endTime = System.nanoTime()
      delta += (endTime - startTime)
    }
    delta /= gs.length*1000
    delta
  }
  println("time:", benchmark() + "micros", "# graph:", gs.length)
  

}


object BenchmarkIntExp extends Benchmark with ArithGraphExp with App with RichImpl with Compile {

  def intToData = (x:scala.Int) => x
  def nextData():scala.Int = prng.nextInt(10)
  def nextInput():scala.IndexedSeq[scala.Int] = (1 to inputSize).map(x => nextData()).toIndexedSeq


  def benchmark(g: Graph): IndexedSeq[Int] => Long = (x:IndexedSeq[Data]) => {
    val startTime: Long = currentNano(g)   
    val out = g(x.toScalaIndexedSeq(inputSize))
    val prt = printlnR(out)
    val endTime: Long = currentNano(prt)
    val delta: Long = endTime - startTime
    delta
  }

  var delta: scala.Long = 0L
  for (g <- gs) {
    println("compiling")
    val compiled = compile(benchmark(g))
    val ni = nextInput()
    delta = delta + compiled(ni).e.asInstanceOf[Const[scala.Long]].x
    println(delta)
  }

  println(delta/1000L/gs.length)

}
