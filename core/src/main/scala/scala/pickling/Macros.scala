package scala.pickling

import scala.pickling.internal._
import ir._


trait TypeAnalysis extends Macro {
  import c.universe._

  def isStaticOnly: Boolean =
    c.inferImplicitValue(typeOf[IsStaticOnly]) != EmptyTree

  def configOption(t: Type): Boolean =
    c.inferImplicitValue(t) != EmptyTree

  def isCaseClass(sym: TypeSymbol): Boolean =
    sym.isClass && sym.asClass.isCaseClass

  def isClosed(sym: TypeSymbol): Boolean =
    whyNotClosed(sym).isEmpty

  def whyNotClosed(sym: TypeSymbol): Seq[String] = {
    if (sym.isEffectivelyFinal)
      Nil
    else if (isCaseClass(sym))
      Nil
    else if (sym.isClass) {
      val classSym = sym.asClass
      if (tools.treatAsSealed(classSym)) {
        tools.directSubclasses(classSym).flatMap(cl => whyNotClosed(cl.asType))
      } else {
        List(s"'${sym.fullName}' allows unknown subclasses (it is not sealed or final isCaseClass=${isCaseClass(sym)} isEffectivelyFinal=${sym.isEffectivelyFinal} isSealed=${classSym.isSealed} directSubclasses=${tools.directSubclasses(classSym)})")
      }
    } else {
      List(s"'${sym.fullName}' is not a class or trait")
    }
  }
}

import HasCompat._

// purpose of this macro: implementation of PickleOps.pickleInto
// This method solely exists to ensure that type checking of builder/pickle format happens.
// We should probably just deprecate/remove the macro as extraneous if we can.
trait PickleMacros extends Macro with TypeAnalysis {
  import c.universe._
  import definitions._

  def pickleTo[T: c.WeakTypeTag](output: c.Tree)(format: c.Tree): c.Tree = {
    val tpe = weakTypeOf[T]
    val q"${_}($pickleeArg)" = c.prefix.tree
    val endPickle = if (shouldBotherAboutCleaning(tpe)) q"clearPicklees()" else q"";
    val pickleeName = newTermName("picklee$pickleTo$")
    val builderName = newTermName("builder$pickleTo$")
    q"""
      import _root_.scala.pickling._
      import _root_.scala.pickling.internal._
      val $pickleeName: $tpe = $pickleeArg
      val $builderName = $format.createBuilder($output)
      _root_.scala.pickling.functions.pickleInto($pickleeName, $builderName)
      $endPickle
    """
  }
}