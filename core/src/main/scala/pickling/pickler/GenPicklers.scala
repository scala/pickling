package scala.pickling
package pickler

import scala.language.experimental.macros

/** Mix-in trait to generate `SPickler`s.
 * See also `SPickler.generate`.
 */
trait GenPicklers {
  implicit def genPickler[T]: SPickler[T] = macro Compat.PicklerMacros_impl[T]
}
