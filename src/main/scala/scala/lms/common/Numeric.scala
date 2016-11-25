package scala.lms
package common

trait Num[A] extends Ord[A] {
  self: A =>

  type B <: BooleanOps[B]

  def +(y: A): A
  def -(y: A): A
  def *(y: A): A
  def /(y: A): A
  def %(y: A): A
}

trait Ord[A] {
  self: A =>

  type B <: BooleanOps[B]

  def >(y: A): B
  def <(y: A): B
  def ===(y: A): B

  def =!=(y: A) = !(self === y)
  def >=(y: A) = !(self < y) 
  def <=(y: A) = !(self > y) 
  

}
