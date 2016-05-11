package scala.pickling.spi

import scala.pickling.{refs, FastTypeTag, Unpickler, Pickler}
import scala.reflect.runtime.universe.Mirror

/** An interface for things which can generate picklers/unpicklers at runtime.
 * TODO - ignore "share"
  */
trait RuntimePicklerGenerator {
  /** Create a new pickler using the given tag. */
  def genPickler(tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_]
  /** Create a new unpickler using the given tag. */
  def genUnpickler(tag: FastTypeTag[_])(implicit share: refs.Share): Unpickler[_]
}
