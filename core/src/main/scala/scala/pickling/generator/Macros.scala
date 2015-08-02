package scala.pickling
package generator

// TODO - move these into Pickler/Unpickler and replace the existing macros.
object PicklingMacros {
  import scala.language.experimental.macros
  def genPickler[T]: Pickler[T] = macro scala.pickling.generator.Compat.genPickler_impl[T]
  def genUnpickler[T]: Unpickler[T] = macro scala.pickling.generator.Compat.genUnpickler_impl[T]
  def genPicklerUnpickler[T]: AbstractPicklerUnpickler[T] = macro scala.pickling.generator.Compat.genPicklerUnpickler_impl[T]
}
trait PicklingMacros extends Macro with SourceGenerator with TypeAnalysis {
  import c.universe._
  val symbols = new IrScalaSymbols[c.universe.type, c.type](c.universe, tools)
  // TODO - We should have more customization than "isStaticOnly"
  val generator =
    if(isStaticOnly) {
      PicklingAlgorithm.create(Seq(new CaseClassPickling(allowReflection = false), AdtPickling, ScalaSingleton))
    } else {
      PicklingAlgorithm.create(Seq(new CaseClassPickling(allowReflection = true), AdtPickling, ScalaSingleton))
    }

  object logger extends AlgorithmLogger {
    def warn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
    def debug(msg: String): Unit =
    // These are enabled when -verbose is enabled.
      c.info(c.enclosingPosition, msg, force=false)
    def abort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
    def error(msg: String): Unit = c.error(c.enclosingPosition, msg)
  }
  def genPickler[T: c.WeakTypeTag]: c.Tree = preferringAlternativeImplicits {
    val tpe = computeType[T]
    val sym = symbols.newClass(tpe)
    val impl = generator.generateImpl(sym, logger)
    val tree2 = impl map {
      case PickleUnpickleImplementation(alg2, alg) => generatePicklerClass[T](alg2)
    }
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate pickler for $tpe")
        ???
      case Some(tree) => tree
    }
  }
  def genUnPickler[T: c.WeakTypeTag]: c.Tree = preferringAlternativeImplicits {
    val tpe = computeType[T]
    val sym = symbols.newClass(tpe)
    val impl = generator.generateImpl(sym, logger)
    val tree2 = impl map {
      case PickleUnpickleImplementation(alg2, alg) => generateUnpicklerClass[T](alg)
    }
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate pickler for $tpe")
        ???
      case Some(tree) => tree
    }
  }
  def genPicklerUnpickler[T: c.WeakTypeTag]: c.Tree = preferringAlternativeImplicits {
    val tpe = computeType[T]
    val sym = symbols.newClass(tpe)
    val impl = generator.generateImpl(sym, logger)
    val tree2 = impl map generatePicklerUnpicklerClass[T]
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate pickler for $tpe")
        ???
      case Some(tree) => tree
    }
  }
}