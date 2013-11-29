package scala.pickling.binary

import scala.collection.mutable.ArrayBuffer
import scala.pickling.EncodingOutput

sealed abstract class ByteBuffer extends EncodingOutput[Array[Byte]] {
  def encodeByteTo(pos: Int, value: Byte): Unit

  def encodeShortTo(pos: Int, value: Short): Unit

  def encodeCharTo(pos: Int, value: Char): Unit

  def encodeIntTo(pos: Int, value: Int): Unit

  def encodeLongTo(pos: Int, value: Long): Unit

  def encodeStringTo(pos: Int, value: String): Int

  def encodeBooleanTo(pos: Int, value: Boolean): Unit

  def encodeByteArrayTo(pos: Int, ia: Array[Byte]): Int

  def encodeShortArrayTo(pos: Int, ia: Array[Short]): Int

  def encodeCharArrayTo(pos: Int, ia: Array[Char]): Int

  def encodeIntArrayTo(pos: Int, ia: Array[Int]): Int

  def encodeLongArrayTo(pos: Int, ia: Array[Long]): Int

  def encodeBooleanArrayTo(pos: Int, ia: Array[Boolean]): Int

  def encodeFloatArrayTo(pos: Int, ia: Array[Float]): Int

  def encodeDoubleArrayTo(pos: Int, ia: Array[Double]): Int

  def copyTo(pos: Int, bytes: Array[Byte]): Unit

  def decodeByteFrom(pos: Int): Byte

  def decodeShortFrom(pos: Int): Short

  def decodeCharFrom(pos: Int): Char

  def decodeIntFrom(pos: Int): Int

  def decodeLongFrom(pos: Int): Long

  def decodeStringFrom(pos: Int): (String, Int)

  def decodeBooleanFrom(pos: Int): Boolean

  def decodeByteArrayFrom(pos: Int): (Array[Byte], Int)

  def decodeShortArrayFrom(pos: Int): (Array[Short], Int)

  def decodeCharArrayFrom(pos: Int): (Array[Char], Int)

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int)

  def decodeLongArrayFrom(pos: Int): (Array[Long], Int)

  def decodeBooleanArrayFrom(pos: Int): (Array[Boolean], Int)

  def decodeFloatArrayFrom(pos: Int): (Array[Float], Int)

  def decodeDoubleArrayFrom(pos: Int): (Array[Double], Int)

  def toArray: Array[Byte]

  def result(): Array[Byte] =
    toArray

  // puts the byte array representing `obj` at the end of the buffer
  def put(obj: Array[Byte]): this.type = ???
}

/* Implementation of `ByteBuffer` using fixed-size array.
 */
final class ByteArray(arr: Array[Byte]) extends ByteBuffer {

  def this(size: Int) {
    this(Array.ofDim[Byte](size))
  }

  def encodeByteTo(pos: Int, value: Byte): Unit =
    arr(pos) = value

  def encodeShortTo(pos: Int, value: Short): Unit =
    Util.encodeShortTo(arr, pos, value)

  def encodeCharTo(pos: Int, value: Char): Unit =
    Util.encodeCharTo(arr, pos, value)

  def encodeLongTo(pos: Int, value: Long): Unit =
    Util.encodeLongTo(arr, pos, value)

  def encodeIntTo(pos: Int, value: Int): Unit =
    Util.encodeIntTo(arr, pos, value)

  def encodeStringTo(pos: Int, value: String): Int =
    Util.encodeStringTo(arr, pos, value)

  def encodeBooleanTo(pos: Int, value: Boolean): Unit =
    Util.encodeBooleanTo(arr, pos, value)

  def encodeByteArrayTo(pos: Int, ia: Array[Byte]): Int =
    Util.encodeByteArrayTo(arr, pos, ia)

  def encodeShortArrayTo(pos: Int, ia: Array[Short]): Int =
    Util.encodeShortArrayTo(arr, pos, ia)

  def encodeCharArrayTo(pos: Int, ia: Array[Char]): Int =
    Util.encodeCharArrayTo(arr, pos, ia)

  def encodeIntArrayTo(pos: Int, ia: Array[Int]): Int =
    Util.encodeIntArrayTo(arr, pos, ia)

  def encodeLongArrayTo(pos: Int, ia: Array[Long]): Int =
    Util.encodeLongArrayTo(arr, pos, ia)

  def encodeBooleanArrayTo(pos: Int, ia: Array[Boolean]): Int =
    Util.encodeBooleanArrayTo(arr, pos, ia)

  def encodeFloatArrayTo(pos: Int, ia: Array[Float]): Int =
    Util.encodeFloatArrayTo(arr, pos, ia)

  def encodeDoubleArrayTo(pos: Int, ia: Array[Double]): Int =
    Util.encodeDoubleArrayTo(arr, pos, ia)

  def copyTo(pos: Int, bytes: Array[Byte]): Unit =
    Util.copy(arr, pos, bytes)

  def decodeByteFrom(pos: Int): Byte =
    arr(pos)

  def decodeShortFrom(pos: Int): Short =
    Util.decodeShortFrom(arr, pos)

  def decodeCharFrom(pos: Int): Char =
    Util.decodeCharFrom(arr, pos)

  def decodeIntFrom(pos: Int): Int =
    Util.decodeIntFrom(arr, pos)

  def decodeLongFrom(pos: Int): Long =
    Util.decodeLongFrom(arr, pos)

  def decodeStringFrom(pos: Int): (String, Int) =
    Util.decodeStringFrom(arr, pos)

  def decodeBooleanFrom(pos: Int): Boolean =
    Util.decodeBooleanFrom(arr, pos)

  def decodeByteArrayFrom(pos: Int): (Array[Byte], Int) =
    Util.decodeByteArrayFrom(arr, pos)

  def decodeShortArrayFrom(pos: Int): (Array[Short], Int) =
    Util.decodeShortArrayFrom(arr, pos)

  def decodeCharArrayFrom(pos: Int): (Array[Char], Int) =
    Util.decodeCharArrayFrom(arr, pos)

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int) =
    Util.decodeIntArrayFrom(arr, pos)

  def decodeLongArrayFrom(pos: Int): (Array[Long], Int) =
    Util.decodeLongArrayFrom(arr, pos)

  def decodeBooleanArrayFrom(pos: Int): (Array[Boolean], Int) =
    Util.decodeBooleanArrayFrom(arr, pos)

  def decodeFloatArrayFrom(pos: Int): (Array[Float], Int) =
    Util.decodeFloatArrayFrom(arr, pos)

  def decodeDoubleArrayFrom(pos: Int): (Array[Double], Int) =
    Util.decodeDoubleArrayFrom(arr, pos)

  def toArray: Array[Byte] =
    arr
}

/* Implementation of `ByteBuffer` using `ArrayBuffer`.
 */
final class ByteArrayBuffer extends ByteBuffer {

  private val buf = ArrayBuffer[Byte]()

  // pos is ignored!
  def encodeByteTo(pos: Int, value: Byte): Unit = {
    buf += value
  }

  // pos is ignored!
  def encodeShortTo(pos: Int, value: Short): Unit = {
    val fst = (value >>> 8 & 0xff).asInstanceOf[Byte]
    val snd = (value & 0xff).asInstanceOf[Byte]
    buf += fst
    buf += snd
  }

  // pos is ignored!
  def encodeCharTo(pos: Int, value: Char): Unit = {
    val fst = (value >>> 8 & 0xff).asInstanceOf[Byte]
    val snd = (value & 0xff).asInstanceOf[Byte]
    buf += fst
    buf += snd
  }

  // pos is ignored!
  def encodeIntTo(pos: Int, value: Int): Unit = {
    assert(buf.size == pos)
    val fst = (value >>> 24).asInstanceOf[Byte]
    val snd = (value >>> 16 & 0xff).asInstanceOf[Byte]
    val thrd = (value >>> 8 & 0xff).asInstanceOf[Byte]
    val frth = (value & 0xff).asInstanceOf[Byte]
    buf += fst
    buf += snd
    buf += thrd
    buf += frth
  }

  // pos is ignored!
  def encodeLongTo(pos: Int, value: Long): Unit = {
    val elem1 = (value >>> 56 & 0xff).asInstanceOf[Byte]
    val elem2 = (value >>> 48 & 0xff).asInstanceOf[Byte]
    val elem3 = (value >>> 40 & 0xff).asInstanceOf[Byte]
    val elem4 = (value >>> 32 & 0xff).asInstanceOf[Byte]
    val elem5 = (value >>> 24 & 0xff).asInstanceOf[Byte]
    val elem6 = (value >>> 16 & 0xff).asInstanceOf[Byte]
    val elem7 = (value >>> 8 & 0xff).asInstanceOf[Byte]
    val elem8 = (value & 0xff).asInstanceOf[Byte]
    buf += elem1
    buf += elem2
    buf += elem3
    buf += elem4
    buf += elem5
    buf += elem6
    buf += elem7
    buf += elem8
  }

  def encodeStringTo(pos: Int, value: String): Int = {
    // assume pos == buf.size
    val bytes = value.getBytes("UTF-8")
    val len   = bytes.length
    encodeIntTo(pos, len)
    buf ++= bytes
    pos + 4 + len
  }

  def encodeBooleanTo(pos: Int, value: Boolean): Unit = {
    buf += (if (value) 1 else 0)
  }

  def encodeByteArrayTo(pos: Int, ia: Array[Byte]): Int = {
    val ba = Array.ofDim[Byte](4 + ia.length * 1)
    val newpos = Util.encodeByteArrayTo(ba, 0, ia)
    buf ++= ba
    pos + newpos
  }

  def encodeShortArrayTo(pos: Int, ia: Array[Short]): Int = {
    val ba = Array.ofDim[Byte](4 + ia.length * 2)
    val newpos = Util.encodeShortArrayTo(ba, 0, ia)
    buf ++= ba
    pos + newpos
  }

  def encodeCharArrayTo(pos: Int, ia: Array[Char]): Int = {
    val ba = Array.ofDim[Byte](4 + ia.length * 2)
    val newpos = Util.encodeCharArrayTo(ba, 0, ia)
    buf ++= ba
    pos + newpos
  }

  def encodeIntArrayTo(pos: Int, ia: Array[Int]): Int = {
    val ba = Array.ofDim[Byte](4 + ia.length * 4)
    val newpos = Util.encodeIntArrayTo(ba, 0, ia)
    buf ++= ba
    pos + newpos
  }

  def encodeLongArrayTo(pos: Int, ia: Array[Long]): Int = {
    val ba = Array.ofDim[Byte](4 + ia.length * 8)
    val newpos = Util.encodeLongArrayTo(ba, 0, ia)
    buf ++= ba
    pos + newpos
  }

  def encodeBooleanArrayTo(pos: Int, ia: Array[Boolean]): Int = {
    val ba = Array.ofDim[Byte](4 + ia.length * 1)
    val newpos = Util.encodeBooleanArrayTo(ba, 0, ia)
    buf ++= ba
    pos + newpos
  }

  def encodeFloatArrayTo(pos: Int, ia: Array[Float]): Int = {
    val ba = Array.ofDim[Byte](4 + ia.length * 4)
    val newpos = Util.encodeFloatArrayTo(ba, 0, ia)
    buf ++= ba
    pos + newpos
  }

  def encodeDoubleArrayTo(pos: Int, ia: Array[Double]): Int = {
    val ba = Array.ofDim[Byte](4 + ia.length * 8)
    val newpos = Util.encodeDoubleArrayTo(ba, 0, ia)
    buf ++= ba
    pos + newpos
  }

  def copyTo(pos: Int, bytes: Array[Byte]): Unit =
    buf ++= bytes

  def decodeByteFrom(pos: Int): Byte =
    ???

  def decodeShortFrom(pos: Int): Short =
    ???

  def decodeCharFrom(pos: Int): Char =
    ???

  def decodeIntFrom(pos: Int): Int =
    ???

  def decodeLongFrom(pos: Int): Long =
    ???

  def decodeStringFrom(pos: Int): (String, Int) =
    ???

  def decodeBooleanFrom(pos: Int): Boolean =
    ???

  def decodeByteArrayFrom(pos: Int): (Array[Byte], Int) =
    ???

  def decodeShortArrayFrom(pos: Int): (Array[Short], Int) =
    ???

  def decodeCharArrayFrom(pos: Int): (Array[Char], Int) =
    ???

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int) =
    ???

  def decodeLongArrayFrom(pos: Int): (Array[Long], Int) =
    ???

  def decodeBooleanArrayFrom(pos: Int): (Array[Boolean], Int) =
    ???

  def decodeFloatArrayFrom(pos: Int): (Array[Float], Int) =
    ???

  def decodeDoubleArrayFrom(pos: Int): (Array[Double], Int) =
    ???

  def toArray: Array[Byte] =
    buf.toArray
}
