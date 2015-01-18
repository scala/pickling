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
   	 // TODO - Don't force pickling lock unless we're pickling into something dynamic.
   	 scala.pickling.internal.GRL.lock()
   	 try {
       if(picklee != null) {
         builder.hintTag(pickler.tag)
         pickler.pickle(picklee, builder)
       } else {
         builder.hintTag(scala.pickling.FastTypeTag.Null)
         scala.pickling.AllPicklers.nullPicklerUnpickler.pickle(null, builder)
       }
     } finally scala.pickling.internal.GRL.unlock()
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
  import scala.reflect.runtime.{universe=>ru}
  // Note: This should be "safe" to use if you guarantee no runtime reflection use, at ALL
  //        in any pickleformat *or* unpickler.
  def unpickleUnsafe[T](thePickle: Pickle, mirror: ru.Mirror)(implicit unpickler: Unpickler[T], format: PickleFormat): T = {
    val reader = format.createReader(
        thePickle.asInstanceOf[format.PickleType],  // Note: Macro is also unsafe here
        mirror)
    val result = unpickler.unpickleEntry(reader)
    // TODO - We should look up whether or not to do this based on available implicits (I think)
    internal.clearUnpicklees()
    result.asInstanceOf[T]
  }
  /** Reconstitutes the data type inside a given Pickle. 
   * @param thePickle  The pickle which contains a piece of data
   * @param mirror   The mirror we use to reflectively restore types, if needed.
   */
  def unpickle[T](thePickle: Pickle, mirror: ru.Mirror)(implicit unpickler: Unpickler[T], format: PickleFormat): T = {
      // We only unpickle in the midst of a lock against reflection, as it's unsafe
      // to use in a threaded context.
      scala.pickling.internal.GRL.lock()
      try unpickleUnsafe(thePickle, mirror)(unpickler, format)
      finally scala.pickling.internal.GRL.unlock()
  }

  implicit class PickleOps[T](picklee: T) {
    //def pickle(implicit format: PickleFormat, pickler: SPickler[T]): format.PickleType = 
    //  scala.pickling.pickle(picklee)(format, pickler)
    def pickle(implicit format: PickleFormat): format.PickleType = 
      macro Compat.PickleMacros_pickle[T]
    def pickleInto(builder: PBuilder)(implicit pickler: SPickler[T]): Unit = 
      //macro Compat.PickleMacros_pickleInto[T]
      scala.pickling.pickleInto(picklee, builder)(pickler)
    def pickleTo[S](output: S)(implicit format: PickleFormat): Unit = 
       macro Compat.PickleMacros_pickleTo[T,S]
  }

  implicit class UnpickleOps(val thePickle: Pickle) {
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T = 
    macro Compat.UnpickleMacros_pickleUnpickle[T]
  }

}
