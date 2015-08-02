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
}
