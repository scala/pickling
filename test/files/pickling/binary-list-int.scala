/*
  Custom pickler/unpickler for type `List`.
*/

import scala.language.higherKinds

import scala.pickling._
import binary._

import reflect.runtime.universe._
import scala.collection.mutable.ListBuffer

class HandwrittenListIntPicklerUnpickler[Coll[_] <: List[_]](val format: PickleFormat)
    extends Pickler[Coll[Int]] with Unpickler[Coll[Int]] {

  def pickle(picklee: Coll[Int], builder: PickleBuilder): Unit = {
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

object Test extends App {

  implicit def genListPickler[Coll[_] <: List[_]](implicit format: PickleFormat): Pickler[Coll[Int]] with Unpickler[Coll[Int]] =
    new HandwrittenListIntPicklerUnpickler(format)

  val l = List[Int](7, 24, 30)
  val pckl = l.pickle
  println(pckl.value.asInstanceOf[Array[Byte]].mkString("[", ",", "]"))
  val res = pckl.unpickle[List[Int]]
  println(res)
}
