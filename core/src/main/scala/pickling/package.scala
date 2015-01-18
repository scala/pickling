package scala

import scala.language.experimental.macros


package object pickling {

   /** Raw/static pickle method which, given an implicit format and Pickler, can serialize
    *  data into the pickle type.
    */
   def pickle[T](picklee: T)(implicit format: PickleFormat, pickler: SPickler[T]): format.PickleType = {
     val builder = format.createBuilder()
     scala.pickling.pickleInto(picklee, builder)
     // TODO - We should look up whether or not to do this based on available implicits (I think)
     internal.clearPicklees()
     builder.result.asInstanceOf[format.PickleType]
   }
   /** Raw/Static method which will feed a given object/type into a builder. */
   def pickleInto[T](picklee: T, builder: PBuilder)(implicit pickler: SPickler[T]): Unit = {
     if(picklee != null) {
        builder.hintTag(pickler.tag)
        pickler.pickle(picklee, builder)
     } else {
        builder.hintTag(scala.pickling.FastTypeTag.Null)
        scala.pickling.AllPicklers.nullPicklerUnpickler.pickle(null, builder)
     }
   }

  // NOTE - The static versoin of this method is WAY more annoying to use than
  //        the convenient macros in PickleOps.
  def pickleTo[T](format: PickleFormat)(picklee: T, output: format.OutputType)(implicit pickler: SPickler[T]): Unit = {
    import scala.pickling._
    import scala.pickling.internal._
    // TODO - This is a hack to convert an inner type into a path-dependent type.
    //        This should *MOSTLY* be ok in the intended pickler usage, but is a hole someone
    //        could trip over, that the macros would fix.
    val builder = format.createBuilder(output)
    scala.pickling.pickleInto(picklee, builder)(pickler)
    // TODO - We should look up whether or not to do this based on available implicits (I think)
    internal.clearPicklees()
  }
  // Note: This should be "safe" to use if you guarantee no runtime reflection use, at ALL
  //        in any pickleformat *or* unpickler.
  def unpickleUnsafe[T](thePickle: Pickle)(implicit unpickler: Unpickler[T], format: PickleFormat, tag: FastTypeTag[T]): T = {
    val reader = format.createReader(
        thePickle.asInstanceOf[format.PickleType],  // Note: Macro is also unsafe here
        scala.pickling.internal.`package`.currentMirror)
        reader.hintTag(unpickler.tag)
        // TODO - should we hint it's statically elided?  That logic should actually
        //        be done in the Unpickler, not in this method.
        //        However, existing macro implementation do that.
        // TODO - Addtionally, we should not do the beginEntry/endEntry.  That should be done
        //        inside the Unpickler, including the FastTypeTag business.
        val typeString = reader.beginEntryNoTag()
        // TODO - if we can get rid of this FastTypeTag here, we can prevent
        //        unsafety for all Unicklers which don't call "beginEntry"
        val result = unpickler.unpickle({ scala.pickling.FastTypeTag(typeString) }, reader)
        reader.endEntry()
        // TODO - We should look up whether or not to do this based on available implicits (I think)
        internal.clearUnpicklees()
        result.asInstanceOf[T]
  }
  /** Reconstitutes the data type inside a given Pickle. */
  def unpickle[T](thePickle: Pickle)(implicit unpickler: Unpickler[T], format: PickleFormat, tag: FastTypeTag[T]): T = {
      // We only unpickle in the midst of a lock against reflection, as it's unsafe
      // to use in a threaded context.
      scala.pickling.internal.GRL.lock()
      try unpickleUnsafe(thePickle)(unpickler, format, tag)
      finally scala.pickling.internal.GRL.unlock()
  }


  // NOTE: PickleOps makes use of macros to inline pickling calls, and do
  //       some fun type/implicit gymnastics.  
  // TODO - can we just use the raw methods?
  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat): format.PickleType = macro Compat.PickleMacros_pickle[T]
    def pickleInto(builder: PBuilder): Unit = macro Compat.PickleMacros_pickleInto[T]
    def pickleTo[S](output: S)(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T,S]
  }

  implicit class UnpickleOps(val thePickle: Pickle) {
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T = macro Compat.UnpickleMacros_pickleUnpickle[T]
  }

}
