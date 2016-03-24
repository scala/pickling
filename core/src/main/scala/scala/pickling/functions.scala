package scala.pickling

object functions {
  def unpickle[T](thePickle: Pickle)(implicit unpickler: Unpickler[T], format: PickleFormat): T = {
    val reader = format.createReader(thePickle.asInstanceOf[format.PickleType])
    val result = unpickler.unpickleEntry(reader).asInstanceOf[T]
    // TODO - some mechanism to disable this.
    internal.clearUnpicklees()
    result
  }

  def pickle[T](picklee: T)(implicit format: PickleFormat, pickler: Pickler[T]): format.PickleType = {
    val builder = format.createBuilder
    pickleInto(picklee, builder)
    // TODO - some mechanism to disable this.
    internal.clearPicklees()
    builder.result.asInstanceOf[format.PickleType]
  }

  // Note: this does NOT clear picklees.
  def pickleInto[T](picklee: T, builder: PBuilder)(implicit pickler: Pickler[T]): Unit = {
    if (null == picklee) Defaults.nullPickler.pickle(null, builder)
    else pickler.pickle(picklee, builder)
  }

  def pickleTo[T, F <: PickleFormat](picklee: T, output: F#OutputType)(implicit pickler: Pickler[T], format: F): Unit = {
    // Lesser HACK POWER TIME! - We should probably find a way of ensuring S <:< format.OutputType...
    val builder = format.createBuilder(output.asInstanceOf[format.OutputType])
    pickleInto(picklee, builder)
    // TODO - some mechanism to turn this off, also should we have the GRL for this?
    internal.clearPicklees()
  }
}
