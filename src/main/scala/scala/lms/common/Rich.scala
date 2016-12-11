package scala.lms
package common

import internal._

import java.io.PrintWriter

import scala.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import scala.lms.util.ClosureCompare

import scala.reflect.SourceContext


trait Rich extends Booleans with Ints with Doubles with Floats with Longs with Arrays with Units with  Functions with IfThenElse with Equals with Lists with IndexedSeqs with Matrixs with Strings

trait RichExp extends Rich with BooleansExp with IntsExp with DoublesExp with FloatsExp with LongsExp with UnitsExp with ArraysExp with FunctionsExp with IfThenElseExp  with EqualsExp with ListsExp with IndexedSeqsExp with MatrixsExp with StringsExp

trait RichImpl extends RichExp with BooleansImpl with IntsImpl with DoublesImpl with FloatsImpl with LongsImpl with UnitsImpl with ArraysImpl with FunctionsExp with IfThenElseExp with EqualsImpl with ListsImpl with MatrixsImpl with IndexedSeqsImpl

trait RichOptImpl extends RichExp with BooleansImpl with IntsOptImpl with DoublesOptImpl with FloatsOptImpl with LongsOptImpl with UnitsImpl with ArraysOptImpl with FunctionsExp with IfThenElseExpOpt with EqualsOptImpl with ListsOptImpl with MatrixsOptImpl with IndexedSeqsOptImpl

trait ScalaGenRich extends ScalaGenIntsExp with ScalaGenLongsExp with ScalaGenDoublesExp with ScalaGenFloatsExp with ScalaGenIfThenElse with ScalaGenFunctions with ScalaGenEquals with ScalaGenLists with ScalaGenMatrixs with ScalaGenIndexedSeqs with ScalaGenUnits with ScalaGenStrings {
  val IR: RichExp
}


