package scala.pickling
package generator

import scala.collection.mutable

import HasCompat._

private[pickling] trait PicklingMacros
    extends Macro with SourceGenerator with TypeAnalysis {

  implicit val implContext = c

  import c.universe._
  import compat._

  val symbols = new IrScalaSymbols[c.universe.type, c.type](c.universe, tools)
  // TODO - also allow import to disable the warnings.
  val disableWillRobinsonWarning =
    java.lang.Boolean.getBoolean("pickling.willrobinson.disablewarning")
  // TODO - We should have more customization than this
  val handleCaseClassSubclasses = !configOption(
      typeOf[IsIgnoreCaseClassSubclasses])
  val generator =
    if (isStaticOnly) {
      PicklingAlgorithm.aggregate(
          Seq(new CaseClassPickling(
                  allowReflection = false,
                  careAboutSubclasses = handleCaseClassSubclasses),
              AdtPickling,
              ScalaSingleton))
    } else {
      PicklingAlgorithm.aggregate(
          Seq(new CaseClassPickling(
                  allowReflection = true,
                  careAboutSubclasses = handleCaseClassSubclasses),
              AdtPickling,
              ScalaSingleton,
              new ExternalizablePickling,
              new WillRobinsonPickling(
                  showWarnings = !disableWillRobinsonWarning)))
    }

  implicit object logger extends AlgorithmLogger {
    def warn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
    def error(msg: String): Unit = c.error(c.enclosingPosition, msg)
    def abort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
    def debug(msg: String): Unit = {
      // These are enabled when -verbose is enabled.
      c.info(c.enclosingPosition, msg, force = false)
    }
  }

  import MacrosErrors._

  def checkClassType(tpe: c.Type, isUnpickle: Boolean = true): Unit = {
    import definitions._
    tpe.normalize match {
      case tpe1 if tpe1.typeSymbol.isClass => () // This case is fine.
      case NothingTpe => impossibleGeneration("Nothing")
      case RefinedType(parents, decls) => impossibleGeneration("refined type")
      case _ if tpe.isScalaOrJavaPrimitive =>
        impossibleGeneration("primitive types")
      case _ => impossibleGeneration(s"non-class type $tpe")
    }
  }

  def preferExistingImplicits(body: => Tree): Tree = {

    import Compat._
    import Console._

    val candidates = c.enclosingImplicits
    if (candidates.isEmpty) return body
    val ourPt = candidates.head.pt

    def debug(msg: Any) = {
      val padding = "  " * (candidates.length - 1)
      //Console.err.println(padding + msg)
    }

    debug(MAGENTA_B + "Can we enter " + ourPt + "?" + RESET)
    debug(candidates)

    if ((candidates.size >= 2) && {
      /* This checks if `inferImplicitValue` has called the same implicit
       * macro and avoids non-termination by circular invocation */
      val theirPt = candidates.tail.head.pt
      ourPt =:= theirPt
    }) {
      debug(RED_B + s"No, parent type is the same $ourPt" + RESET)
      c.abort(c.enclosingPosition, "stepping aside: repeating itself")
    } else {
      debug(YELLOW_B + s"Not sure, need to explore alternatives" + RESET)
      c.inferImplicitValue(ourPt, silent = true) match {
        case success if success != EmptyTree =>
          debug(BLUE_B + s"No, because there's $success" + RESET)
          c.abort(c.enclosingPosition,
                  "stepping aside: there are other candidates")
        case _ =>
          debug(GREEN_B + s"Yes, there are no obstacles. Entering: $ourPt" +
              RESET)
          debug(s"Generated tree: $body")
          body
      }
    }
  }

  def genPicklerUnpickler[T : c.WeakTypeTag]: c.Tree = {
    preferExistingImplicits {
      val tpe = computeType[T]
      checkClassType(tpe)
      val sym = symbols.newClass(tpe)
      val impl = PicklingAlgorithm.run(generator)(sym, logger)
      impl map generatePicklerUnpicklerClass[T] match {
        case Some(tree) => tree
        case None => MacrosErrors.failedGeneration(tpe.toString)
      }
    }
  }
}
