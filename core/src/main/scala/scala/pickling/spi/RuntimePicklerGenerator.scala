package scala.pickling.spi

import scala.pickling.{refs, FastTypeTag, Unpickler, Pickler}
import scala.reflect.runtime.universe.Mirror

/**
 * An interface for things which can generate picklers/unpicklers at runtime.
 */
trait RuntimePicklerGenerator {
  /** Create a new pickler using the given tagKey. */
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_]
  /** Create a new unpickler using the given tagKey. */
  def genUnpickler(mirror: Mirror, key: String)(implicit share: refs.Share): Unpickler[_]
}
