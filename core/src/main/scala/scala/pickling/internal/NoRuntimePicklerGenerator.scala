package scala.pickling
package internal

import scala.pickling.spi.RuntimePicklerGenerator

/** A runtime pickler generator that just throws exceptions when trying to create picklers. */
object NoRuntimePicklerGeneration extends RuntimePicklerGenerator {
  override def genUnpickler(key: String): Unpickler[_] =
    sys.error(s"Runtime pickling generation is disabled, cannot make unpickler for $key")
  override def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_]): Pickler[_] =
    sys.error(s"Runtime pickling generation is disabled, cannot make pickler for $tag")
}