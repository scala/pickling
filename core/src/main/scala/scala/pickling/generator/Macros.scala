package scala.pickling
package generator


trait PicklingMacros extends Macro with SourceGenerator {
  import c.universe._
  val symbols = new IrScalaSymbols[c.universe.type, c.type](c.universe, tools)
  // TODO - create this based on the context. If user doesn't want reflection, disable it.
  val generator = PicklingAlgorithm.create(Seq(new CaseClassPickling(allowReflection = true), AdtPickling))
  object logger extends AlgorithmLogger {
    def warn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
    def debug(msg: String): Unit =
    // These are enabled when -verbose is enabled.
      c.info(c.enclosingPosition, msg, force=false)
    def abort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
    def error(msg: String): Unit = c.error(c.enclosingPosition, msg)
  }
  def genPickler[T: c.WeakTypeTag]: c.Tree = {
    val tpe = computeType[T]
    val sym = symbols.newClass(tpe)
    val impl = generator.generateImpl(sym, logger)
    val tree2 = impl map {
      case PickleUnpickleImplementation(alg2, alg) => generatePicklerClass[T](alg2)
    }
    System.err.println(s"Pickling impl = $tree2")
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate pickler for $tpe")
        ???
      case Some(tree) => tree
    }
  }
  def genUnPickler[T: c.WeakTypeTag]: c.Tree = {
    val tpe = computeType[T]
    val sym = symbols.newClass(tpe)
    val impl = generator.generateImpl(sym, logger)
    val tree2 = impl map {
      case PickleUnpickleImplementation(alg2, alg) => generateUnpicklerClass[T](alg)
    }
    System.err.println(s"Pickling impl = $tree2")
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate pickler for $tpe")
        ???
      case Some(tree) => tree
    }
  }
  def genPicklerUnpickler[T: c.WeakTypeTag]: c.Tree = {
    val tpe = computeType[T]
    val sym = symbols.newClass(tpe)
    val impl = generator.generateImpl(sym, logger)
    val tree2 = impl map generatePicklerUnpicklerClass[T]
    //System.err.println(s"Pickling impl = $tree2")
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate pickler for $tpe")
        ???
      case Some(tree) => tree
    }
  }
}