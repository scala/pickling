package scala.pickling
package generator


trait PicklingMacros extends Macro with SourceGenerator {
  import c.universe._
  val symbols = new IrScalaSymbols[c.universe.type, c.type](c.universe, tools)
  val generator = PicklingAlgorithm.create(Seq(CaseClassPickling, AdtPickling))
  object logger extends AlgorithmLogger {
    def warn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
    def debug(msg: String): Unit =
    // These are enabled when -verbose is enabled.
      c.info(c.enclosingPosition, msg, force=false)
    def abort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
  }
  def test[T: c.WeakTypeTag]: c.Tree = {
    val tpe = computeType[T]
    val sym = symbols.newClass(tpe)
    val impl = generator.generate(sym, logger)
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
  def picklerUnpickler[T: c.WeakTypeTag]: c.Tree = {
    val tpe = computeType[T]
    val sym = symbols.newClass(tpe)
    val impl = generator.generate(sym, logger)
    val tree2 = impl map generatePicklerUnpicklerClass[T]
    System.err.println(s"Pickling impl = $tree2")
    tree2 match {
      case None =>
        c.error(c.enclosingPosition, s"Failed to generate pickler for $tpe")
        ???
      case Some(tree) => tree
    }
  }
}