package scala.pickling.generator.scalasymbols

import scala.pickling.Macro
import scala.pickling.generator.{IrSymbol, IrScalaSymbols}
import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}
import scala.language.experimental.macros


trait SymbolTestMacros extends Macro {
  import c.universe._
  val symbols = new IrScalaSymbols[c.universe.type, c.type](c.universe, tools)

  def constructorParamTypes[T: WeakTypeTag]: Seq[String] = {
    val tpe = weakTypeOf[T]
    val cls = symbols.newClass(tpe)
    cls.primaryConstructor match {
      case Some(x) => x.parameterTypes[c.universe.type](c.universe).toList.flatMap(_.map(_.toString))
      case None => Seq()
    }
  }

  def varTypes[T: WeakTypeTag]: Seq[String] = {
    val tpe = weakTypeOf[T]
    val cls = symbols.newClass(tpe)
    //System.err.println(s"Vartypes($tpe, ${tpe.key}})")
    // TODO - distinct?
    val mthds = IrSymbol.allDeclaredMethodIncludingSubclasses(cls)
    mthds.filter(_.isVar).map { x =>
      //System.err.println(s" - $x, ${x.returnType(c.universe)} from ${x.owner}")
      x.returnType[c.universe.type](c.universe).toString
    }
  }

  def fieldNames[T: WeakTypeTag]: Seq[String] = {
    val tpe = weakTypeOf[T]
    val cls = symbols.newClass(tpe)
    cls.fields.map(_.fieldName)
  }
  def varNames[T: WeakTypeTag]: Seq[String] = {
    val tpe = weakTypeOf[T]
    val cls = symbols.newClass(tpe)
    val mthds = IrSymbol.allDeclaredMethodIncludingSubclasses(cls)
    mthds.filter(_.isVar).map(_.methodName)
  }
  def valNames[T: WeakTypeTag]: Seq[String] = {
    val tpe = weakTypeOf[T]
    val cls = symbols.newClass(tpe)
    //System.err.println(s"Checking $tpe members:\n - ${cls.methods.map(x => s"${x}, isVal: ${x.isVal}, isVar: ${x.isVar}, isParam: ${x.isParamAccessor}").mkString("\n - ")}")
    cls.methods.filter(_.isVal).map(_.methodName)
  }
  def paramNames[T: WeakTypeTag]: Seq[String] = {
    val tpe = weakTypeOf[T]
    val cls = symbols.newClass(tpe)
    cls.methods.filter(_.isParamAccessor).map(_.methodName)
  }

  def transientFields[T : WeakTypeTag]: Seq[String] = {
    val tpe = weakTypeOf[T]
    val cls = symbols.newClass(tpe)
    cls.fields.filter(_.isMarkedTransient).map(_.fieldName)
  }
  def transientVars[T : WeakTypeTag]: Seq[String] = {
    val tpe = weakTypeOf[T]
    val cls = symbols.newClass(tpe)
    cls.methods.filter { x =>
      //System.err.println(s"Checking var/val/param for $x")
      x.isVar || x.isVal || x.isParamAccessor
    }.filter { x =>
      //System.err.println(s"Checking $x for transient: ${x.isMarkedTransient}")
      x.isMarkedTransient
    }.map(_.methodName)
  }

  def parentClasses[T: WeakTypeTag]: Seq[String] = {
    val tpe = weakTypeOf[T]
    val cls = symbols.newClass(tpe)
    cls.parentClasses.map(_.tpe[c.universe.type](c.universe).toString)
  }
}

object Compat {
  def constructorParamTypes[T]: Seq[String] = macro constructorParamTypes_impl[T]
  def constructorParamTypes_impl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with SymbolTestMacros
    import c.universe._
    c.Expr[Seq[String]](
      q"""Seq[String](..${bundle.constructorParamTypes[T]})"""
    )
  }
  def varTypes[T]: Seq[String] = macro getVarTypes_impl[T]
  def getVarTypes_impl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with SymbolTestMacros
    import c.universe._
    c.Expr[Seq[String]](
      q"""Seq[String](..${bundle.varTypes[T]})"""
    )
  }

  def parentClassTags[T]: Seq[String] = macro getParentClassTags_impl[T]
  def getParentClassTags_impl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with SymbolTestMacros
    import c.universe._
    c.Expr[Seq[String]](
      q"""Seq[String](..${bundle.parentClasses[T]})"""
    )
  }

  def getTransientFieldNames[T]: Seq[String] = macro getTransientFieldNames_Impl[T]
  def getTransientFieldNames_Impl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with SymbolTestMacros
    import c.universe._
    c.Expr[Seq[String]](
      q"""Seq[String](..${bundle.transientFields[T]})"""
    )
  }
  def getTransientVars[T]: Seq[String] = macro getTransientVars_Impl[T]
  def getTransientVars_Impl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with SymbolTestMacros
    import c.universe._
    c.Expr[Seq[String]](
      q"""Seq[String](..${bundle.transientVars[T]})"""
    )
  }
  def fieldNames[T]: Seq[String] = macro getFieldNames_Impl[T]
  def getFieldNames_Impl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with SymbolTestMacros
    import c.universe._
    c.Expr[Seq[String]](
      q"""Seq[String](..${bundle.fieldNames[T]})"""
    )
  }
  def varNames[T]: Seq[String] = macro varNames_impl[T]
  def varNames_impl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with SymbolTestMacros
    import c.universe._
    c.Expr[Seq[String]](
      q"""Seq[String](..${bundle.varNames[T]})"""
    )
  }
  def valNames[T]: Seq[String] = macro valNames_impl[T]
  def valNames_impl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with SymbolTestMacros
    import c.universe._
    c.Expr[Seq[String]](
      q"""Seq[String](..${bundle.valNames[T]})"""
    )
  }
  def paramNames[T]: Seq[String] = macro paramNames_impl[T]
  def paramNames_impl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with SymbolTestMacros
    import c.universe._
    c.Expr[Seq[String]](
      q"""Seq[String](..${bundle.paramNames[T]})"""
    )
  }
}
