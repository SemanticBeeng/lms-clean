package scala.lms
package internal

import java.io.PrintWriter
import collection.mutable.HashSet

trait CLikeCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._

  def mangledName(name: String) = name.replaceAll("\\s","").map(c => if(!c.isDigit && !c.isLetter) '_' else c) 

  // List of datastructure types that requires transfer functions to be generated for this target
  val dsManifestesList = HashSet[(Manifest[_],String)]()

  // Streams for helper functions and its header
  var helperFuncStream: PrintWriter = _
  var headerStream: PrintWriter = _
  var actRecordStream: PrintWriter = _
  var typesStream: PrintWriter = _

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = emitValDef(sym, rhs)

  def emitValDef(sym: Sym[Any], rhs: String): Unit = emitValDef(quote(sym), sym.tp, rhs)

  def emitValDef(sym: String, tpe: Manifest[_], rhs: String): Unit = {
    if(remap(tpe) != "void") stream.println(remap(tpe) + " " + sym + " = " + rhs + ";")
  }

  override def emitVarDecl(sym: Sym[Any]): Unit = {
    stream.println(remap(sym.tp) + " " + quote(sym) + ";")
  }

  override def emitAssignment(sym: Sym[Any], rhs: String): Unit = {
    stream.println(quote(sym) + " = " + rhs + ";")
  }

  def remapWithRef[A](m: Manifest[A]): String = remap(m) + addRef(m)
  def remapWithRef(tpe: String): String = tpe + addRef(tpe)

  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[AnyVal]])
      remap(m.typeArguments.head)
    else if (m.erasure == classOf[List[Any]]) { // Use case: Delite Foreach sync list 
      deviceTarget.toString + "List< " + remap(m.typeArguments.head) + " >"
    }
    else {
      m.toString match {
        case "scala.collection.immutable.List[Float]" => "List"
        case "Boolean" => "bool"
        case "Byte" => "int8_t"
        case "Char" => "uint16_t"
        case "Short" => "int16_t"
        case "Int" => "int32_t"
        case "Long" => "int64_t"
        case "Float" => "float"
        case "Double" => "double"
        case "Unit" => "void"
        case "Nothing" => "void"
        case _ => throw new GenerationFailedException("CLikeGen: remap(m) : Manifeste %s cannot be remapped.".format(m.toString))
      }
    }
  }

  def addRef(): String = if (cppMemMgr=="refcnt") " " else " *"
  def addRef[A](m: Manifest[A]): String = addRef(remap(m))
  def addRef(tpe: String): String = {
    if (!isPrimitiveManifeste(tpe) && !isVoidManifeste(tpe)) addRef()
    else " "
  }
  
  // move to CCodegen?
  def unwrapSharedPtr(tpe: String): String = {
    assert(cppMemMgr == "refcnt")
    if(tpe.contains("std::shared_ptr")) 
      tpe.replaceAll("std::shared_ptr<","").replaceAll(">","") 
    else 
      tpe
  }
  def wrapSharedPtr(tpe: String): String = {
    assert(cppMemMgr == "refcnt")
    if(!isPrimitiveManifeste(tpe) && !isVoidManifeste(tpe)) 
      "std::shared_ptr<" + tpe + ">" 
    else 
      tpe
  }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultManifeste: String, resultIsVar: Boolean, external: Boolean): Unit = {

    stream.append("#include \"" + deviceTarget + "helperFuncs.h\"\n")
    
    def kernelSignature: String = {
      val out = new StringBuilder
      if(resultIsVar) {
        if (cppMemMgr == "refcnt")
          out.append(wrapSharedPtr(hostTarget + "Ref" + unwrapSharedPtr(resultManifeste)))
        else
          out.append(hostTarget + "Ref" + resultManifeste + addRef())
      }
      else {
        out.append(resultManifeste + addRef(resultManifeste))
      }

      out.append(" kernel_" + syms.map(quote).mkString("") + "(")
      out.append(vals.map(p => remap(p.tp) + " " + addRef(p.tp) + quote(p)).mkString(", "))
      if (vals.length > 0 && vars.length > 0) {
        out.append(", ")
      }
      if (vars.length > 0) {
        if (cppMemMgr == "refcnt")
          out.append(vars.map(v => wrapSharedPtr(hostTarget + "Ref" + unwrapSharedPtr(remap(v.tp))) + " " + quote(v)).mkString(","))
        else
          out.append(vars.map(v => hostTarget + "Ref" + remap(v.tp) + addRef() + " " + quote(v)).mkString(","))
      }
      out.append(")")
      out.toString
    }

    //TODO: Remove the dependency to Multiloop to Delite
    if (!resultManifeste.startsWith("DeliteOpMultiLoop")) {
      stream.println(kernelSignature + " {")
      headerStream.println(kernelSignature + ";")
    }
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultManifeste: String, resultIsVar: Boolean, external: Boolean): Unit = {
    //TODO: Remove the dependency to Multiloop to Delite
    if(resultManifeste != "void" && !resultManifeste.startsWith("DeliteOpMultiLoop"))
      stream.println("return " + quote(syms(0)) + ";")

    if(!resultManifeste.startsWith("DeliteOpMultiLoop"))
      stream.println("}")
/*
    for(s <- syms++vals++vars) {
      if(dsManifestesList.contains(s.tp)) println("contains :" + remap(s.tp))
      else println("not contains: " + remap(s.tp))
    }
    println(syms.map(quote).mkString("") + "adding dsManifestesList:" + (syms++vals++vars).map(_.tp).mkString(","))
    dsManifestesList ++= (syms++vals++vars).map(_.tp)
    println("dsManifests-lms:" + dsManifestesList.map(remap(_)).mkString(",")) //toString)
  */
    dsManifestesList ++= (syms++vals++vars).map(s => (s.tp,remap(s.tp)))
  }

  def isPrimitiveManifeste(tpe: String) : Boolean = {
    tpe match {
      case "bool" | "int8_t" | "uint16_t" | "int16_t" | "int32_t" | "int64_t" | "float" | "double" => true
      case _ => false
    }
  }

  def isVoidManifeste(tpe: String) : Boolean = {
    if(tpe == "void") true
    else false
  }

  
  def CLikeConsts(x:Exp[Any], s:String): String = {
    s match {
      case "Infinity" => "std::numeric_limits<%s>::max()".format(remap(x.tp))
      case _ => super.quote(x)
    }
  }
  
  override def quote(x: Exp[Any]) = x match {
    case Const(s: Unit) => ""
    case Const(s: Float) => s+"f"
    case Const(null) => "NULL"
    case Const(z) => CLikeConsts(x, z.toString)
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
}

trait CLikeNestedCodegen extends GenericNestedCodegen with CLikeCodegen {
  val IR: Expressions with Effects
  import IR._
}

trait CLikeFatCodegen extends GenericFatCodegen with CLikeCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
}
