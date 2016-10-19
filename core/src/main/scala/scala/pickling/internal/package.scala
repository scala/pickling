package scala.pickling

import java.util.concurrent.atomic.AtomicReference

import scala.language.experimental.macros
import scala.language.reflectiveCalls

package object internal {

  import scala.reflect.runtime.{universe => ru}
  import spi._

  private[this] def initDefaultRuntime = {
    // TODO - Figure out a better way to configure this.... (typesafe config?)
    sys.props.getOrElse("pickling.runtime", "default") match {
      case "hybrid" => new HybridRuntime()
      case "noreflection" => new NoReflectionRuntime()
      case _ => new DefaultRuntime()

    }
  }
  private[this] val currentRuntimeVar = new AtomicReference[PicklingRuntime](initDefaultRuntime)
  def currentRuntime: PicklingRuntime = currentRuntimeVar.get

  /** Replace the old [[PicklingRuntime]] keeping its state. This operation
    * is not thread-safe and it's expected to be executed in a single thread.
    *
    * Note that we don't do anything with the [[RefRegistry]] because in
    * future versions we are going to change how cyclic references work.
    */
  def replaceRuntime(r: PicklingRuntime): Unit = {
    if(r.picklers.isLookupEnabled) {
      currentRuntime.picklers.dumpStateTo(r.picklers)
    }
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

  // ----- utilities for managing object identity -----
  @deprecated("Use `currentRuntime.refRegistry.pickle.registerPicklee` instead", "0.11")
  def lookupPicklee(picklee: Any): Int = currentRuntime.refRegistry.pickle.registerPicklee(picklee)
  @deprecated("Use `currentRuntime.refRegistry.pickle.registerPicklee` instead", "0.11")
  def registerPicklee(picklee: Any): Int = currentRuntime.refRegistry.pickle.registerPicklee(picklee)
  @deprecated("Use `currentRuntime.refRegistry.pickle.clear` instead", "0.11")
  def clearPicklees() = currentRuntime.refRegistry.pickle.clear()
  @deprecated("Use `currentRuntime.refRegistry.unpickle.lookupUnpicklee` instead", "0.11")
  def lookupUnpicklee(index: Int): Any = currentRuntime.refRegistry.unpickle.lookupUnpicklee(index)
  @deprecated("Use `currentRuntime.refRegistry.unpickle.preregisterUnpicklee` instead", "0.11")
  def preregisterUnpicklee() = currentRuntime.refRegistry.unpickle.preregisterUnpicklee()
  @deprecated("Use `currentRuntime.refRegistry.unpickle.registerUnpicklee` instead", "0.11")
  def registerUnpicklee(unpicklee: Any, index: Int) = currentRuntime.refRegistry.unpickle.registerUnpicklee(index, unpicklee)
  @deprecated("Use `currentRuntime.refRegistry.unpickle.clear` instead", "0.11")
  def clearUnpicklees() = currentRuntime.refRegistry.unpickle.clear()
}
