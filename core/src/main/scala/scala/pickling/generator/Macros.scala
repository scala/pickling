package scala.pickling
package generator

// TODO - move these into Pickler/Unpickler and replace the existing macros.
private[pickling] object PicklingMacros {
  import scala.language.experimental.macros
  def genPickler[T]: Pickler[T] with Generated = macro scala.pickling.generator.Compat.genPickler_impl[T]
  def genUnpickler[T]: Unpickler[T] with Generated = macro scala.pickling.generator.Compat.genUnpickler_impl[T]
  def genPicklerUnpickler[T]: AbstractPicklerUnpickler[T] with Generated = macro scala.pickling.generator.Compat.genPicklerUnpickler_impl[T]
}
private[pickling] trait PicklingMacros extends Macro with SourceGenerator with TypeAnalysis {
  import c.universe._
  val symbols = new IrScalaSymbols[c.universe.type, c.type](c.universe, tools)
  // TODO - We should have more customization than this
  val handleCaseClassSubclasses = !configOption(typeOf[IsIgnoreCaseClassSubclasses])
  val generator =
    if(isStaticOnly) {
      // TODO - should we consider externalizable "safe" or "static only" since we know it's externalizable at compile time?
      PicklingAlgorithm.aggregate(Seq(new CaseClassPickling(allowReflection = false, careAboutSubclasses = handleCaseClassSubclasses), AdtPickling, ScalaSingleton))
    } else {
      PicklingAlgorithm.aggregate(Seq(
        new CaseClassPickling(allowReflection = true, careAboutSubclasses = handleCaseClassSubclasses),
        AdtPickling,
        ScalaSingleton,
        new ExternalizablePickling,
        WillRobinsonPickling))
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

  def genPickler[T: c.WeakTypeTag]: c.Tree = preferringAlternativeImplicits {
    val tpe = computeType[T]
    checkClassType(tpe)
    val sym = symbols.newClass(tpe)
    val impl = PicklingAlgorithm.run(generator)(sym, logger)
    val tree2 = impl map {
      case PickleUnpickleImplementation(alg2, alg) => generatePicklerClass[T](alg2)
    }
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate pickler for $tpe")
        ???
      case Some(tree) =>
        //System.err.println(s" --=== $tpe ===--\n$tree\n --=== / $tpe ===--")
        tree
    }
  }
  def genUnPickler[T: c.WeakTypeTag]: c.Tree = preferringAlternativeImplicits {
    val tpe = computeType[T]
    checkClassType(tpe)
    val sym = symbols.newClass(tpe)
    val impl = PicklingAlgorithm.run(generator)(sym, logger)
    val tree2 = impl map {
      case PickleUnpickleImplementation(alg2, alg) => generateUnpicklerClass[T](alg)
    }
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate unpickler for $tpe")
        ???
      case Some(tree) =>
        //System.err.println(s" --=== $tpe ===--\n$tree\n --=== / $tpe ===--")
        tree
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