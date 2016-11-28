package scala.lms
package common

trait AddTimeAble[A] {
  self: A =>
  def *(y: A): A
  def +(y: A): A

}
trait Num[A] extends Ord[A] with AddTimeAble[A] {
  self: A =>

  def -(y: A): A
  def /(y: A): A
  def %(y: A): A
}

trait Ord[A] {
  self: A =>

  type B <: BooleanOps[B]

  def >(y: A): B
  def <(y: A): B

  def >=(y: A) = !(self < y) 
  def <=(y: A) = !(self > y) 
  
  def min(x: A): A
  def max(x: A): A  

}
