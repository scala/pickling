package scala.pickling
package pickler

import scala.language.experimental.macros

// This is no longer needed, we think.
trait GenOpenSumUnpicklers {
  //implicit def genOpenSumUnpickler[T]: Unpickler[T] with Generated = macro Compat.OpenSumUnpicklerMacro_impl[T]
}

trait GenUnpicklers extends GenOpenSumUnpicklers {
  implicit def genUnpickler[T]: AbstractPicklerUnpickler[T] with Generated =
    macro generator.Compat.genPicklerUnpickler_impl[T  ]
}
