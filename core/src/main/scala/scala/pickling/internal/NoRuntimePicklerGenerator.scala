package scala.pickling
package internal

import scala.pickling.refs.Share
import scala.pickling.spi.RuntimePicklerGenerator
import scala.reflect.runtime.universe.Mirror

/** A runtime pickler generator that just throws exceptions when trying to create picklers. */
object NoRuntimePicklerGeneration extends RuntimePicklerGenerator {
  override def genUnpickler(mirror: Mirror, key: String)(implicit share: refs.Share): _root_.scala.pickling.Unpickler[_] =
      sys.error(s"Runtime pickling generation is disabled, cannot make pickler for $key")
  /** Create a new pickler using the given tagKey. */
  override def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit share: Share): Pickler[_] =
    sys.error(s"Runtime pickling generation is disabled, cannot make unpickler for $clazz")
}