/*
  Custom pickler/unpickler test.

  Custom pickler/unpickler for type `List`, currently assuming Scala binary format
  (although typically custom picklers/unpicklers don't need to be specialized on a PickleFormat)
*/

import scala.pickling._
import binary._
import reflect.runtime.universe._
import scala.collection.mutable.ListBuffer

package scala.pickling {
  class Custom
}

object Test extends App {

  implicit def genListPickler[T](implicit format: PickleFormat): HandwrittenListIntPicklerUnpickler = new HandwrittenListIntPicklerUnpickler
  class HandwrittenListIntPicklerUnpickler(implicit val format: PickleFormat) extends Pickler[List[Int]] with Unpickler[List[Int]] {
    def pickle(picklee: List[Int], builder: PickleBuilder): Unit = {
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
      builder.endCollection(i)
      builder.endEntry()
    }
    def unpickle(tag: => TypeTag[_], reader: PickleReader): Any = {
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

  val l = List[Int](7, 24, 30)
  val pckl = l.pickle
  println(pckl.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))
  val res = pckl.unpickle[List[Int]]
  println(res)
}
