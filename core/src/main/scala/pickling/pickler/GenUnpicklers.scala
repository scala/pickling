package scala.pickling.pickler

import scala.pickling._
import scala.language.experimental.macros

trait GenOpenSumUnpicklers {
  implicit def genOpenSumUnpickler[T]: Unpickler[T] with Generated = macro Compat.OpenSumUnpicklerMacro_impl[T]
}

trait GenUnpicklers extends GenOpenSumUnpicklers {
  implicit def genUnpickler[T]: Unpickler[T] with Generated = macro Compat.UnpicklerMacros_impl[T]
}

object GenUnpicklers extends GenUnpicklers {}
