package scala

import scala.language.experimental.macros


package object pickling {

  def unpickle[T](thePickle: Pickle)(implicit unpickler: Unpickler[T], format: PickleFormat): T = {
  	// TODO - move GRL locking code into just the runtime unpickler code + generators.
  	internal.GRL.lock()
  	try {
  	  val reader = format.createReader(thePickle.asInstanceOf[format.PickleType])
  	  val result = unpickler.unpickleEntry(reader).asInstanceOf[T]
  	  // TODO - some mechanism to disable this.
  	  internal.clearUnpicklees();
  	  result
  	} finally internal.GRL.unlock()
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
  	// TODO - Move GRL Lock only into dynamic picklers
  	internal.GRL.lock()
  	try { 
  	  if(null == picklee) {
  	    builder.hintTag(FastTypeTag.Null)
  	    all.nullPicklerUnpickler.pickle(null, builder)
  	  } else {
  	    builder.hintTag(pickler.tag)
  	    pickler.pickle(picklee, builder)
  	  }
  	  } finally internal.GRL.unlock()
    // TODO - This usually doesn't clear Picklee's, but should we?
  }

  def pickleTo[T, F <: PickleFormat](picklee: T, output: F#OutputType)(implicit pickler: SPickler[T], format: F): Unit = {
  	// Lesser HACK POWER TIME! - We should probably find a way of ensuring S <:< format.OutputType...
  	val builder = format.createBuilder(output.asInstanceOf[format.OutputType])
  	pickleInto(picklee, builder)
  	// TODO - some mechanism to turn this off, also should we have the GRL for this?
  	internal.clearPicklees()
  }

  /** Appends the pickle/pickleTo/pickleInto operations onto any type, assuming implicits picklers are available. */
  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat, pickler: SPickler[T]): format.PickleType =
      scala.pickling.pickle[T](picklee)(format, pickler)
    def pickleInto(builder: PBuilder)(implicit pickler: SPickler[T]): Unit =
      scala.pickling.pickleInto(picklee, builder)(pickler)
    // pickleTo remains a macro so further type-checking of [S <:< format.OutputType] occurs,
    // and any implicit conversions required for this.
    def pickleTo[S](output: S)(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T,S]
    //def pickleTo[S](output: S)(implicit format: PickleFormat, pickler: SPickler[T]): Unit =
    //  scala.pickling.pickleTo(picklee, output)(pickler, format)
  }

  implicit class UnpickleOps(val thePickle: Pickle) {
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T =
       // TODO - Ideally we get a compiler error if pickle type doesn't match.
       scala.pickling.unpickle(thePickle.asInstanceOf[format.PickleType])(unpickler, format)
  }

}
