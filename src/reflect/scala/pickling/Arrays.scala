package scala.pickling

import scala.reflect.runtime.universe._

class ArrayPickler[T: TypeTag]
  (implicit pf: PickleFormat) extends Pickler[Array[T]] with Unpickler[Array[T]] {

  val format: PickleFormat = pf

  def pickle(arr: Array[T], builder: PickleBuilder): Unit = {
    builder.hintStaticallyElidedType()
    builder.beginEntry(arr)
    builder.endEntry()
  }

  def unpickle(tag: => TypeTag[_], reader: PickleReader): Any = {
    reader.readArray()
  }

}
