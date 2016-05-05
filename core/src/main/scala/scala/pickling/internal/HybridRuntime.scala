package scala.pickling.internal

import java.util.concurrent.locks.ReentrantLock

import scala.reflect.runtime
import scala.pickling.FastTypeTag
import scala.pickling.spi.{RefRegistry, PicklingRuntime}

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
