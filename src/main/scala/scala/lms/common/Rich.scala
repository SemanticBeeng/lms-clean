package scala.lms
package common

import internal._

import java.io.PrintWriter

import scala.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import scala.lms.util.ClosureCompare

import scala.reflect.SourceContext


trait Rich extends Primitives with  Functions with IfThenElse

trait RichImpl extends Rich with PrimitivesImpl with FunctionsExp with IfThenElsePureExp

trait ScalaGenRich extends ScalaGenPrimitives with ScalaGenIfThenElsePure with ScalaGenFunctions{
  val IR: RichImpl
}
