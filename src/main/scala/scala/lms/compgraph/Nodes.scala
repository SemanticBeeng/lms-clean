package scala.lms
package compgraph

trait Nodes {

  type Data
  type Input = List[Data]
  type Output = Data


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

  def OutputNode() = new OutputNode {}

  trait InputNode extends Node {
    val inputSize = 0
    def op(input: Input) =
      throw new Exception("Is not a processing node")
  }

  def InputNode() = new OutputNode {}  

}


trait DerivableNodes extends Nodes {

  def zero: Data
  def one: Data

  trait Node extends super.Node {

    def derivative(input: Input) = {
      require(input.length == inputSize)
      val r = d(input)
      require(r.length == inputSize)
      r
    }

    protected def d(input: Input): List[Data]    
    
  }

  trait OutputNode extends super.OutputNode {
    
    def d(input: Input) =
      List(one)
  }

  override def OutputNode() = new OutputNode {}

  trait InputNode extends super.InputNode {

    def d(input: Input) =
      throw new Exception("Is not a processing node")
  }

  override def InputNode() = new OutputNode {}
  

}
