package scala

import scala.language.experimental.macros


package object pickling {

  def unpickle[T](thePickle: Pickle)(implicit unpickler: Unpickler[T], format: PickleFormat): T = {
  	val reader = format.createReader(thePickle.asInstanceOf[format.PickleType])
  	val result = unpickler.unpickleEntry(reader).asInstanceOf[T]
  	// TODO - some mechanism to disable this.
  	internal.clearUnpicklees();
  	result
  }
  def pickle[T](picklee: T)(implicit format: PickleFormat, pickler: SPickler[T]): format.PickleType = {
  	val builder = format.createBuilder
  	pickleInto(picklee, builder)
  	// TODO - some mechanism to disable this.
  	internal.clearPicklees()
  	builder.result.asInstanceOf[format.PickleType]
  }
  def pickleInto[T](picklee: T, builder: PBuilder)(implicit pickler: SPickler[T]): Unit = {
  	// TODO - BeginEntry/EndEntry needed?
  	// TODO - this hinting should be in the pickler, not here.  We need to understand
  	//        when we want to use this vs. something else.
  	if(null == picklee) {
  	  builder.hintTag(FastTypeTag.Null)
  	  all.nullPicklerUnpickler.pickle(null, builder)
  	} else {
  	  builder.hintTag(pickler.tag)
  	  pickler.pickle(picklee, builder)
  	}
    // TODO - This usually doesn't clear Picklee's, but should we?
    // TODO - GRL Lock
  }
  def pickleTo[T, S](picklee: T, output: S)(implicit pickler: SPickler[T], format: PickleFormat): Unit = {
  	// SUPER HACK POWER TIME! - We should probably find a way of ensuring S <:< format.OutputType...
  	val builder = format.createBuilder(output.asInstanceOf[format.OutputType])

  	pickleInto(picklee, builder)
  	// TODO - some mechanism to turn this off
  	internal.clearPicklees()
  }

  implicit class PickleOps[T](picklee: T) {

    //def pickle(implicit format: PickleFormat): format.PickleType = macro Compat.PickleMacros_pickle[T]
    def pickle(implicit format: PickleFormat, pickler: SPickler[T]): format.PickleType =
      scala.pickling.pickle[T](picklee)(format, pickler)
    // Note: TONS of macros in picklers use this, so we leave it as is...
    //def pickleInto(builder: PBuilder): Unit = macro Compat.PickleMacros_pickleInto[T]
    def pickleInto(builder: PBuilder)(implicit pickler: SPickler[T]): Unit =
      scala.pickling.pickleInto(picklee, builder)(pickler)
    def pickleTo[S](output: S)(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T,S]
  }

  implicit class UnpickleOps(val thePickle: Pickle) {
    //def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T = macro Compat.UnpickleMacros_pickleUnpickle[T]
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T =
       // TODO - Ideally we get a compiler error if pickle type doesn't match.
       scala.pickling.unpickle(thePickle.asInstanceOf[format.PickleType])(unpickler, format)
  }

}
