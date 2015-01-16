package scala.pickling.pickler

import scala.pickling._
import scala.language.experimental.macros

/** Mix-in trait to generate `SPickler`s.
 * See also `SPickler.generate`.
 */
trait GenPicklers {
  implicit def genPickler[T]: SPickler[T] = macro Compat.PicklerMacros_impl[T]
}

object GenPicklers extends GenPicklers {}
