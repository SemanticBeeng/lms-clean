package scala.lms
package compgraph

trait BooleanNodes extends Nodes {

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
