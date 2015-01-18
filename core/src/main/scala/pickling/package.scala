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
  	pickler.pickle(picklee, builder)
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
    def pickle(implicit format: PickleFormat): format.PickleType = macro Compat.PickleMacros_pickle[T]
    def pickleInto(builder: PBuilder): Unit = macro Compat.PickleMacros_pickleInto[T]
    def pickleTo[S](output: S)(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T,S]
  }

  implicit class UnpickleOps(val thePickle: Pickle) {
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T = macro Compat.UnpickleMacros_pickleUnpickle[T]
  }

}
