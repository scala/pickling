package scala.pickling
package generator

private[pickling] object PicklingMacros {
  import scala.language.experimental.macros

  @deprecated("Use `genPicklerUnpickler` instead", "0.11")
  def genPickler[T]: AbstractPicklerUnpickler[T] with Generated =
    macro scala.pickling.generator.Compat.genPicklerUnpickler_impl[T]

  @deprecated("Use `genPicklerUnpickler` instead", "0.11")
  def genUnpickler[T]: AbstractPicklerUnpickler[T] with Generated =
    macro scala.pickling.generator.Compat.genPicklerUnpickler_impl[T]

  def genPicklerUnpickler[T]: AbstractPicklerUnpickler[T] with Generated =
    macro scala.pickling.generator.Compat.genPicklerUnpickler_impl[T]
}
private[pickling] trait PicklingMacros extends Macro with SourceGenerator with TypeAnalysis {
  import c.universe._
  val symbols = new IrScalaSymbols[c.universe.type, c.type](c.universe, tools)
  // TODO - also allow import to disable the warnings.
  val disableWillRobinsonWarning = java.lang.Boolean.getBoolean("pickling.willrobinson.disablewarning")
  // TODO - We should have more customization than this
  val handleCaseClassSubclasses = !configOption(typeOf[IsIgnoreCaseClassSubclasses])
  val generator =
    if(isStaticOnly) {
      PicklingAlgorithm.aggregate(Seq(
        new CaseClassPickling(allowReflection = false, careAboutSubclasses = handleCaseClassSubclasses),
        AdtPickling,
        ScalaSingleton))
    } else {
      PicklingAlgorithm.aggregate(Seq(
        new CaseClassPickling(allowReflection = true, careAboutSubclasses = handleCaseClassSubclasses),
        AdtPickling,
        ScalaSingleton,
        new ExternalizablePickling,
        new WillRobinsonPickling(showWarnings = !disableWillRobinsonWarning)))
    }

  object logger extends AlgorithmLogger {
    def warn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
    def debug(msg: String): Unit =
    // These are enabled when -verbose is enabled.
      c.info(c.enclosingPosition, msg, force=false)
    def abort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
    def error(msg: String): Unit = c.error(c.enclosingPosition, msg)
  }

  def checkClassType(tpe: c.Type, isUnpickle: Boolean = true): Unit = {
    import definitions._
    tpe.normalize match {
      case NothingTpe =>
        c.abort(c.enclosingPosition, "cannot generate pickling logic for type Nothing")
      case RefinedType(parents, decls) =>
        c.abort(c.enclosingPosition, "cannot generate pickling logic for refined type")
      case _ if tpe.isEffectivelyPrimitive || tpe.typeSymbol == StringClass =>
        c.abort(c.enclosingPosition, s"cannot generate pickling logic for primitive type: $tpe")
      case tpe1 if tpe1.typeSymbol.isClass =>
        ()  // This case is fine.
      case _ =>
        c.abort(c.enclosingPosition, s"cannot generate pickling logic for non-class type $tpe")
    }
  }

  def genPicklerUnpickler[T: c.WeakTypeTag]: c.Tree = preferringAlternativeImplicits {
    val tpe = computeType[T]
    checkClassType(tpe)
    val sym = symbols.newClass(tpe)
    val impl = PicklingAlgorithm.run(generator)(sym, logger)
    //System.err.println(impl)
    val tree2 = impl map generatePicklerUnpicklerClass[T]
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate pickler/unpickler for $tpe")
        ???
      case Some(tree) =>
        //System.err.println(s" --=== $tpe ===--\n$tree\n --=== / $tpe ===--")
        tree
    }
  }
}
