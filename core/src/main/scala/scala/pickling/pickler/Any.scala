package scala.pickling
package pickler

/** Attempts to unpickle Any by looking up registered unpicklers using `currentMirror`.
 */
trait AnyUnpicklers {
  // Any
  implicit val anyUnpickler: Unpickler[Any] = new Unpickler[Any] {
    def unpickle(tag: String, reader: PReader): Any = {
      val actualUnpickler = internal.currentRuntime.picklers.genUnpickler(scala.reflect.runtime.currentMirror, tag)
      actualUnpickler.unpickle(tag, reader)
    }
    def tag: FastTypeTag[Any] = FastTypeTag[Any]
  }
  // Note this is NOT implicit.  This is to be used when constructing picklers for unknown type parameters (e.g.
  // when attempting to lookup runtime handlers).
  val anyPickler: Pickler[Any] = new Pickler[Any] {
    override def pickle(picklee: Any, builder: PBuilder): Unit = {
      // Here we just look up the pickler.
      val clazz = picklee.getClass
      val classLoader = this.getClass.getClassLoader
      internal.GRL.lock()
      val tag = try FastTypeTag.mkRaw(clazz, scala.reflect.runtime.universe.runtimeMirror(classLoader))
                finally internal.GRL.unlock()
      val p = internal.currentRuntime.picklers.genPickler(classLoader, clazz, tag)
      p.asInstanceOf[Pickler[Any]].pickle(picklee, builder)
    }
    override def tag: FastTypeTag[Any] = FastTypeTag[Any]
  }
}
