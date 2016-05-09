package scala.pickling
package pickler

import scala.language.experimental.macros

/** Mix-in trait to generate `Pickler`s and `Unpickler`s implicitly.
  * See also `Pickler.generate` and `Unpickler.generate`.
  */
trait GenPicklersUnpicklers {

  implicit def genPickler[T]: AbstractPicklerUnpickler[T] =
    macro generator.Compat.genPicklerUnpickler_impl[T]

  def genUnpickler[T]: AbstractPicklerUnpickler[T] with Generated =
    macro generator.Compat.genPicklerUnpickler_impl[T]

  // The only implicit definition, otherwise there's a clash
  def genPicklerUnpickler[T]: AbstractPicklerUnpickler[T] with Generated =
    macro generator.Compat.genPicklerUnpickler_impl[T]

}

