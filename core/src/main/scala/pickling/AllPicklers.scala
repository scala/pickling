package scala.pickling

import scala.language.experimental.macros


object AllPicklers extends CorePicklersUnpicklers

object all extends CorePicklersUnpicklers {

  // TODO - why is this here and a duplicate of package.scala in pickling?
  // TODO - should we delegate to raw-methods in scala.pickling package object?
  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat): format.PickleType = macro Compat.PickleMacros_pickle[T]
    def pickleInto(builder: PBuilder): Unit = macro Compat.PickleMacros_pickleInto[T]
    def pickleTo[S](output: S)(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T,S]
  }

  implicit class UnpickleOps(val thePickle: Pickle) {
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T = macro Compat.UnpickleMacros_pickleUnpickle[T]
  }

}
