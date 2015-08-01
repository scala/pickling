package scala.pickling.spi

import scala.pickling.{FastTypeTag, Unpickler, Pickler}

/**
 * An interface for things which can generate picklers/unpicklers at runtime.
 */
trait RuntimePicklerGenerator {
  /** Create a new pickler using the given tagKey. */
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_]): Pickler[_]
  /** Create a new unpickler using the given tagKey. */
  def genUnpickler(tagKey: String): Unpickler[_]
}
