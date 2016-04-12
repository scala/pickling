package scala.pickling
package internal

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import scala.pickling.spi._
import scala.reflect.runtime.universe.Mirror

/**
 * The default implementation of a pickling runtime.
 *
 * Notes:
 *   - This supports circular reference handling via TLS buffers during pickling/unpickling
 *   - This supports runtime pickler/unpickler generation via scala reflection.
 *   - This uses an actual lock to keep reflective usages safe.
 */
class DefaultRuntime extends spi.PicklingRuntime {
  override val GRL = new ReentrantLock()
  /** Gives access to the current refRegistry. */
  override val refRegistry: RefRegistry = new DefaultRefRegistry

  /** Creates a new fastTypeTag with the given tagKey.
    *
    * NOTE; this only assumes the T lines up.
    */
  override def makeFastTag[T](tagKey: String): FastTypeTag[T] = FastTypeTag.apply(tagKey).asInstanceOf[FastTypeTag[T]]

  /** The current reflection mirror to use when doing runtime unpickling/pickling. */
  override def currentMirror: Mirror = _root_.scala.reflect.runtime.currentMirror

  /** A registry of picklers for runtime lookup/usage. */
  override val picklers: PicklerRegistry = new DefaultPicklerRegistry(new DefaultRuntimePicklerGenerator(GRL))
}
