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

  case object OutputNode extends Node {
    val inputSize = 1
    def op(input: Input) =
      input.head
  }

  case object InputNode extends Node {
    val inputSize = 0
    def op(input: Input) =
      throw new Exception("Is not a processing node")
  }

}


