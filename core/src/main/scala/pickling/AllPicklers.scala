package scala.pickling

import scala.language.experimental.macros


object AllPicklers extends CorePicklersUnpicklers

object all extends CorePicklersUnpicklers {

  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat, pickler: SPickler[T]): format.PickleType =
      scala.pickling.pickle[T](picklee)(format, pickler)
    def pickleInto(builder: PBuilder)(implicit pickler: SPickler[T]): Unit =
      scala.pickling.pickleInto(picklee, builder)(pickler)
    def pickleTo[S](output: S)(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T,S]
  }

  implicit class UnpickleOps(val thePickle: Pickle) {
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T =
       // TODO - Ideally we get a compiler error if pickle type doesn't match.
       scala.pickling.unpickle(thePickle.asInstanceOf[format.PickleType])(unpickler, format)
  }

}
