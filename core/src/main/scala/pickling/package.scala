package scala

import scala.language.experimental.macros


package object pickling {

  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat): format.PickleType = macro Compat.PickleMacros_pickle[T]
    def pickleInto(builder: PBuilder): Unit = macro Compat.PickleMacros_pickleInto[T]
    def pickleTo(output: Output[_])(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T]
  }

  implicit class UnpickleOps(pickle: Pickle) {
    def unpickle[T]: T = macro Compat.UnpickleMacros_pickleUnpickle[T]
  }

}
