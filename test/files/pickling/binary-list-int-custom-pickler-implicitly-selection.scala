import scala.pickling._
import binary._

import scala.reflect.runtime.universe._
import scala.collection.mutable.ListBuffer

object Test extends App {
  val lst = (1 to 10).toList

  implicit def genListPickler[T](implicit format: PickleFormat): HandwrittenListIntPicklerUnpickler = new HandwrittenListIntPicklerUnpickler
  class HandwrittenListIntPicklerUnpickler(implicit val format: PickleFormat) extends Pickler[::[Int]] with Unpickler[::[Int]] {
    def pickle(picklee: ::[Int], builder: PickleBuilder): Unit = {
      builder.beginEntry()
      val arr = picklee.toArray
      val length = arr.length
      builder.beginCollection(arr.length)
      builder.hintStaticallyElidedType()
      builder.hintTag(typeTag[Int])
      builder.pinHints()

      var i: Int = 0
      while (i < length) {
        builder.beginEntry(arr(i))
        builder.endEntry()
        i = i + 1
      }

      builder.unpinHints()
      builder.endCollection()
      builder.endEntry()
    }
    def unpickle(tag: TypeTag[_], reader: PickleReader): Any = {
      val arrReader = reader.beginCollection()
      arrReader.hintStaticallyElidedType()
      arrReader.hintTag(typeTag[Int])
      arrReader.pinHints()

      val buffer = ListBuffer[Int]()
      val length = arrReader.readLength()
      var i = 0
      while (i < length) {
        arrReader.beginEntry()
        buffer += arrReader.readPrimitive().asInstanceOf[Int]
        arrReader.endEntry()
        i = i + 1
      }

      arrReader.unpinHints()
      arrReader.endCollection()
      buffer.toList
    }
  }

  val pickle = lst.pickle
  println(pickle)
  println(pickle.unpickle[List[Int]])
}
