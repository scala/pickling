package scala.pickling.pickler

import scala.pickling._
import PicklingErrors.NothingIsNotUnpicklable

trait NothingPicklers {
  implicit object NothingPicklerUnpickler extends AbstractPicklerUnpickler[Nothing] {
    override def tag: FastTypeTag[Nothing] = FastTypeTag.Nothing
    /** Impossible to call in Scala, no value can be typed [[Nothing]] */
    override def pickle(picklee: Nothing, builder: PBuilder): Unit = ???
    /** Don't call [[unpickle]], [[Nothing]] cannot be unpickled. */
    override def unpickle(tag: String, reader: PReader): Any =
      throw NothingIsNotUnpicklable
  }
}

