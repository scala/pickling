package scala.pickling

import java.util.concurrent.atomic.AtomicReference

import scala.language.experimental.macros
import scala.language.reflectiveCalls

import java.util.IdentityHashMap

import HasCompat._

package object internal {

  import scala.reflect.runtime.{universe => ru}
  import ru._
  import compat._

  private[this] def initDefaultRuntime = {
    // TODO - Figure out a better way to configure this.... (typesafe config?)
    sys.props.getOrElse("pickling.runtime", "default") match {
      case "hybrid" => new HybridRuntime()
      case "noreflection" => new NoReflectionRuntime()
      case _ => new DefaultRuntime()

    }
  }
  private[this] var currentRuntimeVar = new AtomicReference[spi.PicklingRuntime](initDefaultRuntime)
  def currentRuntime: spi.PicklingRuntime = currentRuntimeVar.get
  // Here we inject a new runtime for usage.
  def replaceRuntime(r: spi.PicklingRuntime): Unit = {
    currentRuntimeVar.lazySet(r)
  }

  /* Global reflection lock.
   * It is used to avoid data races that typically lead to runtime exceptions
   * when using (Scala) runtime reflection on Scala 2.10.x.
   *
   * Note: visibility must be public, so that the lock can be accessed from
   *       macro-generated code.
   */
  def GRL = currentRuntime.GRL

  // TOGGLE DEBUGGING
  private val debugEnabled: Boolean = System.getProperty("pickling.debug", "false").toBoolean
  private[pickling] def debug(output: => String) = if (debugEnabled) println(output)


  // ----- internal extension methods for symbols -----
  private[pickling] implicit class RichSymbol(sym: scala.reflect.api.Universe#Symbol) {
    def isEffectivelyFinal = sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isEffectivelyFinal
    def isEffectivelyPrimitive = throw new Exception("use Type.isEffectivelyPrimitive instead")
    def isNotNullable = sym.isClass && (sym.asClass.isPrimitive || sym.asClass.isDerivedValueClass)
    def isNullable = sym.isClass && !isNotNullable
  }
  def currentMirror: ru.Mirror = currentRuntime.currentMirror

  

  // FIXME: duplication wrt Tools, but I don't really fancy abstracting away this path-dependent madness
  //private[pickling] 
  implicit class RichTypeFIXME(tpe: Type) {
    import definitions._
    // TODO - this can be removed IFF we migrate runtime picklers to always generate
    //        FastTypeTags using java reflection.
    def key: String = {
      tpe.normalize match {
        case ExistentialType(tparams, TypeRef(pre, sym, targs))
        if targs.nonEmpty && targs.forall(targ => tparams.contains(targ.typeSymbol)) =>
          TypeRef(pre, sym, Nil).key
        case TypeRef(pre, sym, targs) if pre.typeSymbol.isModuleClass =>
          sym.fullName +
          (if (sym.isModuleClass) ".type" else "") +
          (if (targs.isEmpty) "" else targs.map(_.key).mkString("[", ",", "]"))
        // TODO - more robust handling of with types.
        case t  if t.toString contains "with" => 
          t.toString.replaceAll(" with .*", "")
        case t =>
          t.toString
      }
    }
    // TODO - this can probably be moved to FastTypeTag
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.typeSymbol.isClass && eltpe.typeSymbol.asClass.isPrimitive => true
      case _ => false
    }
  }


  // ----- utilities for managing object identity -----
  @deprecated("Use `currentRuntime.refRegistry.pickle.registerPicklee` instead", "0.11")
  def lookupPicklee(picklee: Any): Int = currentRuntime.refRegistry.pickle.registerPicklee(picklee)
  @deprecated("Use `currentRuntime.refRegistry.pickle.registerPicklee` instead", "0.11")
  def registerPicklee(picklee: Any): Int = currentRuntime.refRegistry.pickle.registerPicklee(picklee)
  @deprecated("Use `currentRuntime.refRegistry.pickle.clear` instead", "0.11")
  def clearPicklees() = currentRuntime.refRegistry.pickle.clear()
  @deprecated("Use `currentRuntime.refRegistry.unpickle.lookupUnpicklees` instead", "0.11")
  def lookupUnpicklee(index: Int): Any = currentRuntime.refRegistry.unpickle.lookupUnpicklee(index)
  @deprecated("Use `currentRuntime.refRegistry.unpickle.preregisterUnpicklee` instead", "0.11")
  def preregisterUnpicklee() = currentRuntime.refRegistry.unpickle.preregisterUnpicklee()
  @deprecated("Use `currentRuntime.refRegistry.unpickle.registerUnpicklee` instead", "0.11")
  def registerUnpicklee(unpicklee: Any, index: Int) = currentRuntime.refRegistry.unpickle.regsiterUnpicklee(index, unpicklee)
  @deprecated("Use `currentRuntime.refRegistry.unpickle.clear` instead", "0.11")
  def clearUnpicklees() = currentRuntime.refRegistry.unpickle.clear()
}
