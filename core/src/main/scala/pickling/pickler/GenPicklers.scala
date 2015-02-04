package scala.pickling
package pickler

import scala.language.experimental.macros

/** Mix-in trait to generate `Pickler`s implicitly.
 * See also `Pickler.generate`.
 */
trait GenPicklers {
  implicit def genPickler[T]: Pickler[T] = macro Compat.PicklerMacros_impl[T]
}
