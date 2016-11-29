package scala.lms
package common

import internal._

import java.io.PrintWriter

import scala.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import scala.lms.util.ClosureCompare

import scala.reflect.SourceContext


trait Rich extends Booleans with Ints with Doubles with Floats with Arrays with Units with  Functions with IfThenElse with Equals with Lists with Matrixs

trait RichExp extends Rich with BooleansExp with IntsExp with DoublesExp with FloatsExp with UnitsExp with ArraysExp with FunctionsExp with IfThenElseExp  with EqualsExp with ListsExp with MatrixsExp

trait RichImpl extends RichExp with BooleansImpl with IntsImpl with DoublesImpl with FloatsImpl with UnitsImpl with ArraysImpl with FunctionsExp with IfThenElseExp with EqualsImpl with ListsImpl with MatrixsImpl

trait RichOptImpl extends RichExp with BooleansImpl with IntsOptImpl with DoublesOptImpl with FloatsOptImpl with UnitsImpl with ArraysOptImpl with FunctionsExp with IfThenElseExpOpt with EqualsOptImpl with ListsOptImpl with MatrixsOptImpl

trait ScalaGenRich extends ScalaGenIntsExp with ScalaGenIfThenElse with ScalaGenFunctions with ScalaGenEquals with ScalaGenLists with ScalaGenMatrixs{
  val IR: RichExp
}


