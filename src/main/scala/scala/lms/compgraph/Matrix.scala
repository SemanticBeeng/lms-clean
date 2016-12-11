package scala.lms
package compgraph

import common._


trait MatrixGraphs extends Graphs {
  self: Rich =>

  type NumM <: Num[NumM]
  type Data = Matrix[NumM]
  type Size = (scala.Int, scala.Int)
  type N = MatrixNode
  type G = MatrixGraph
  

  def inputMatrixSize: scala.IndexedSeq[Size]

  trait MatrixNode extends Node {

    def size(linput: scala.List[Size]): Option[Size]

  }

  case object AddNode extends MatrixNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) + input(1)

    def size(input: scala.List[Size]) = {
      val (a, b) = input(0)
      val (c, d) = input(1)      
      if (a.equals(c) && b.equals(d))
        Some((a,b))
      else
        None
      }
  }

  case object MinusNode extends MatrixNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) - input(1)

    def size(input: scala.List[Size]) = {
      val (a, b) = input(0)
      val (c, d) = input(1)
      if (a.equals(c) && b.equals(d))
        Some((a,b))
      else
        None
      }        
  }


  case object TimesNode extends MatrixNode {
    val inputSize = 2
    def op(input: Input) =
      input(0) * input(1)

    def size(input: scala.List[Size]) = {
      val (a, b) = input(0)
      val (c, d) = input(1)      
      if (b.equals(c))
        Some((a,d))
      else
        None
      }
  }

  case object HStackNode extends MatrixNode {
    val inputSize = 2
    def op(input: Input) =
      input(0).hstack(input(1))

    def size(input: scala.List[Size]) = {
      val (a, b) = input(0)
      val (c, d) = input(1)      
      if (b.equals(d))
        Some((a+c,d))
      else
        None
      }
  }

  case object VStackNode extends MatrixNode {
    val inputSize = 2
    def op(input: Input) =
      input(0).vstack(input(1))

    def size(input: scala.List[Size]) = {
      val (a, b) = input(0)
      val (c, d) = input(1)      
      if (a.equals(c))
        Some((a,b+d))
      else
        None
      }
  }
  

  case class ConstantNode(c: Data, val h: scala.Int, val w: scala.Int) extends MatrixNode {
    val inputSize = 0
    def op(input: Input) =
      c

    def size(input: scala.List[Size]) = Some((h, w))
  }

  trait OutputNode extends MatrixNode with super.OutputNode{
    def size(input: scala.List[Size]) = Some(input(0))
  }

  def OutputNode():N = new OutputNode {}

  trait InputNode extends MatrixNode with super.InputNode

  private var iN = -1
  def InputNode():N = {
    iN += 1
    new InputNode {
      var index = iN
      def size(input: scala.List[Size]) = Some(inputMatrixSize(index))
    }
  }
  

  trait MatrixGraph extends Graph {

    checkDim()

    def checkDim() {
      var dim: Map[String, Size] =
        (1 to inputSize)
          .map("IN"+_)
          .map(x => (x, nodes(x)._1.size(scala.List()).get))
          .toMap
      def rec(str:String):Size = {
        if (dim.contains(str)) {
          dim(str)
        } else {
          val (n, i) = nodes(str)
          val sizeInput = i.map(rec).toList          
          val r = n.size(sizeInput).getOrElse(
            throw new Exception("Size doesn't check at node: " +n + " with inputs: " + sizeInput)
          )
          println(str, r)
          dim += ((str, r))
          r
        }
      }
      rec("OUT")
    }
    //checkDim at creation

  }

  def Graph(m: Map[String, R], iS: scala.Int) = new MatrixGraph {
    def nodes = m
    def inputSize = iS 
  }
  

}


trait MatrixGraph extends MatrixGraphs  {
  self: Rich =>

  type NumM = Int

  val inputMatrixSize = IndexedSeq((2,2), (2,2), (2,2))
  def simpleCG = {
    val l = scala.List(GraphNode("ADD", AddNode, scala.List("IN1", "IN2")))
    newGraph(l, 2, "ADD")
  }

  def funCG = {
    val l = scala.List(
      GraphNode("ADD", AddNode, scala.List("IN1", "IN2")),
      GraphNode("ADD2", TimesNode, scala.List("ADD", "ADD")),
      GraphNode("ADD3", AddNode, scala.List("ADD2", "IN3"))              
    )
    newGraph(l, 3, "ADD3")
  }
  

  def app(b: Int): Matrix[Int] = {
    val a:Matrix[NumM] = IndexedSeq(IndexedSeq(b,b), IndexedSeq(b,b))
    //simple(scala.List(a, b))
    funCG(scala.IndexedSeq(a, a, a), false)
    //    add(a, a)
  }
  

}

