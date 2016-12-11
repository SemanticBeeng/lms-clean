package scala.lms
package common


trait Strings extends Base {

  trait StringOps[A] {
    def +(x:A): A
  }

  type StringR <: StringOps[StringR]
  implicit def stringTyp: Rep[StringR] { type Internal = java.lang.String }
  implicit def stringLift: Lift[java.lang.String, StringR]
  def string: Exp[java.lang.String] => StringR
}

trait StringsExp extends BaseExp with Strings {

  case class StringAppend(e1: Exp[java.lang.String], e2: Exp[java.lang.String]) extends Def[java.lang.String]
  case class ToString(e: Exp[Any]) extends Def[java.lang.String]
  case class StringR(e: Exp[java.lang.String]) extends Expressable[java.lang.String] with StringOps[StringR] {
    def +(x: StringR) = string(StringAppend(e, x.e))
  }

  def string: Exp[Any] => StringR = (x:Exp[Any]) => StringR(ToString(x))

  private val repE = RepE[java.lang.String, StringR](string)
  implicit val stringTyp: Rep[StringR] { type Internal = java.lang.String } = repE
  implicit val stringLift: Lift[java.lang.String, StringR] = repE  


}

trait ScalaGenStrings extends ScalaGenBase {
  val IR: StringsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringAppend(a, b) => emitValDef(sym, "" + quote(a) + "+" + quote(b))
    case ToString(a) => emitValDef(sym, "" + quote(a) + ".toString")      
    case _ => super.emitNode(sym, rhs)
  }

}
