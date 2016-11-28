/*package scala.lms
package compgraph

trait BoolGraphs {
  self: SimpleGraph=>

  type Data
  type Input = List[Data]
  type Output = Data
  
  def and(a:Data, b:Data): Data
  def or(a:Data, b:Data): Data

  case object AndNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      and(input(0), input(1))
  }

  case object OrNode extends Node {
    val inputSize = 2
    def op(input: Input) =
      or(input(0), input(1))
  }
  
}
 */
