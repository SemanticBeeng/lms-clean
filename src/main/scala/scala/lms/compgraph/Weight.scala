/*package scala.lms
package compgraph

import common._


trait WeightedNodes extends Nodes {


  type Weights[A]

  def weights: Weights[Data]

  def getWeight(weights: Weights[Data], index: Int): Data

  def add(a:Data, b:Data): Data

  trait WeightedNode extends Node{
    def weightsIndex: List[Int]

    override def output(input: Input) = {
      require(input.length == inputSize)
      op(input, weightsIndex.map(getWeight(weights, _)))
    }

    protected def op(input: Input, weights: Input): Output

    protected def op(input: Input): Output =
      throw new Exception("Must provide weights")
    
  }

  case class BiasNode(weightIndex: Int) extends WeightedNode {

    val inputSize = 1
    def weightsIndex = List(weightIndex)

    def op(input: Input, weights: Input) =
      add(input(0), weights(0))
  }

}
 */
