package scala

import scala.language.experimental.macros


package object pickling {

  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat, pickler: SPickler[T]): format.PickleType = macro Compat.PickleMacros_pickle[T]
    def pickleInto(builder: PBuilder)(implicit pickler: SPickler[T]): Unit = macro Compat.PickleMacros_pickleInto[T]
    def pickleTo(output: Output[_])(implicit format: PickleFormat, pickler: SPickler[T]): Unit = macro Compat.PickleMacros_pickleTo[T]
  }

  implicit class UnpickleOps(val thePickle: Pickle) {
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T = macro Compat.UnpickleMacros_pickleUnpickle[T]
  }

}
