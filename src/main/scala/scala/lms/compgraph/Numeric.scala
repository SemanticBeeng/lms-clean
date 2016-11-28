package scala.lms
package compgraph

import common._

object Numeric {
  case class BooleanO(x: Boolean) extends BooleanOps[BooleanO] {
    type A = BooleanO
    def &&(y: => A): A = BooleanO(x && y.x)
    def ||(y: => A): A = BooleanO(x || y.x)
    def unary_! : A = BooleanO(!x)
  }

  case class IntNum(x: Int) extends Num[IntNum] {
    type A = IntNum
    type B = BooleanO
    def +(y: A): A = IntNum(x + y.x)
    def -(y: A): A = IntNum(x - y.x)
    def *(y: A): A = IntNum(x * y.x)
    def /(y: A): A = IntNum(x / y.x)
    def %(y: A): A = IntNum(x % y.x)
    def ===(y: A): B = BooleanO(x == y.x)
    def >(y: A): B = BooleanO(x > y.x)
    def <(y: A): B = BooleanO(x < y.x)
    def min(y: A): A = IntNum(x.min(y.x))
    def max(y: A): A = IntNum(x.max(y.x))
  }


}
