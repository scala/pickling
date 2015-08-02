package scala.pickling.spi

import java.util.concurrent.locks.ReentrantLock

import scala.pickling.FastTypeTag
import scala.reflect.runtime.universe.Mirror

/**
 * This trait provides access to all the runtime services used in pickling.
 */
trait PicklingRuntime {
  /** Lock used to secure reflection usage. */
  def GRL: ReentrantLock  // TODO - Use some abstraction so that one day we could have a no-lock variant for a runtime that doesn't allow reflection.
  /** Gives access to the current refRegistry. */
  def refRegistry: RefRegistry
  /** Creates a new fastTypeTag with the given tagKey. */
  def makeFastTag[T](tagKey: String): FastTypeTag[T]
  /** A registry of picklers for runtime lookup/usage. */
  def picklers: PicklerRegistry

  /** The current reflection mirror to use when doing runtime unpickling/pickling. */
  //  TODO - Allow this to get modified!
  def currentMirror: Mirror
}
