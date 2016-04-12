package scala.pickling
package pickler

/** An pickler for "Any" value (will look up pickler at runtime, or generate it. */
object AnyPickler extends Pickler[Any] {
  override def pickle(picklee: Any, builder: PBuilder): Unit = {
    // Here we just look up the pickler.
    val clazz = picklee.getClass
    val classLoader = this.getClass.getClassLoader
    internal.GRL.lock()
    val tag = try FastTypeTag.makeRaw(clazz)
    finally internal.GRL.unlock()
    val p = internal.currentRuntime.picklers.genPickler(classLoader, clazz, tag)
    p.asInstanceOf[Pickler[Any]].pickle(picklee, builder)
  }
  override def tag: FastTypeTag[Any] = FastTypeTag.Any
  override def toString = "AnyPickler"
}
/** An unpickler for "Any" value (will look up unpickler at runtime, or generate it. */
object AnyUnpickler extends Unpickler[Any] {
  def unpickle(tag: String, reader: PReader): Any = {
    val actualUnpickler = internal.currentRuntime.picklers.genUnpickler(scala.reflect.runtime.currentMirror, tag)
    actualUnpickler.unpickle(tag, reader)
  }
  def tag: FastTypeTag[Any] = FastTypeTag.Any
  override def toString = "AnyUnPickler"
}

/** Attempts to unpickle Any by looking up registered unpicklers using `currentMirror`.
 */
trait AnyUnpicklers {
  // Any
  implicit val anyUnpickler: Unpickler[Any] = AnyUnpickler
}
