package scala.pickling
package pickler

import scala.language.experimental.macros

/** Mix-in trait to generate `Pickler`s implicitly.
 * See also `Pickler.generate`.
 */
trait GenPicklers {
  implicit def genPickler[T]: AbstractPicklerUnpickler[T] =
    macro generator.Compat.genPicklerUnpickler_impl[T]
}
