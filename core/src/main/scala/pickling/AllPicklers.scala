package scala.pickling

import scala.language.experimental.macros


object AllPicklers extends CorePicklersUnpicklers

object all extends CorePicklersUnpicklers {

  implicit class PickleOps[T: SPickler](picklee: T) {
    def pickle(implicit format: PickleFormat): format.PickleType = {
      val builder = format.createBuilder()
      pickleInto(builder)
      // TODO - clear picklees()?  Check with Heather/Phillip on when this should be done.
      builder.result.asInstanceOf[format.PickleType]
    }
    def pickleInto(builder: PBuilder): Unit = {
      if(picklee != null) {
        // TODO - grab pickler
        val pickler = implicitly[scala.pickling.SPickler[T]]
        builder.hintTag(pickler.tag)
        pickler.pickle(picklee, builder)
      } else {
        builder.hintTag(scala.pickling.FastTypeTag.Null)
        scala.pickling.AllPicklers.nullPicklerUnpickler.pickle(null, builder)
      }
    }
    // NOTE: We leave this as a macro in the hopes that the type system will figure out if
    // PickleFormat.OutputType <:< S
    // We could possibly directly encode this via another mechanism.
    def pickleTo[S](output: S)(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T,S]
  }

  implicit class UnpickleOps(val thePickle: Pickle) {
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T = macro Compat.UnpickleMacros_pickleUnpickle[T]
  }

}
