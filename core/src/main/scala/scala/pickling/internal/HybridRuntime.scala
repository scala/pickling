package scala.pickling.internal

import java.util.concurrent.locks.ReentrantLock

import scala.pickling.internal.DefaultPicklerRegistry
import scala.pickling.refs.Share
import scala.pickling.{Unpickler, Pickler, FastTypeTag, refs}
import scala.pickling.spi.{RefRegistry, PicklerRegistry, PicklingRuntime}
import scala.reflect.runtime

/**
 * This runtime will not use reflection to generate new picklers, but DOES allow lookup of statically generated
 * picklers at runtime.
 */
class HybridRuntime extends PicklingRuntime {
  override def currentMirror: runtime.universe.Mirror = runtime.currentMirror
  override val picklers  = new DefaultPicklerRegistry(NoRuntimePicklerGeneration)
  override val refRegistry: RefRegistry = new DefaultRefRegistry()
  override val GRL: ReentrantLock = new ReentrantLock()
  override def makeFastTag[T](tagKey: String): FastTypeTag[T] = FastTypeTag.apply(tagKey).asInstanceOf[FastTypeTag[T]]
}
