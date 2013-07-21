package scala.pickling.binary

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayBuilder
import scala.pickling.EncodingOutput

sealed abstract class ByteBuffer extends EncodingOutput[Array[Byte]] {
  def encodeByteTo(pos: Int, value: Byte): Int

  def encodeByteAtEnd(pos: Int, value: Byte): Unit

  def encodeShortAtEnd(pos: Int, value: Short): Unit

  def encodeCharAtEnd(pos: Int, value: Char): Unit

  def encodeIntAtEnd(pos: Int, value: Int): Unit

  def encodeLongAtEnd(pos: Int, value: Long): Unit

  def encodeIntTo(pos: Int, value: Int): Int

  def encodeStringTo(pos: Int, value: String): Int

  def encodeBooleanTo(pos: Int, value: Boolean): Int

  def encodeByteArrayTo(pos: Int, ia: Array[Byte]): Int

  def encodeShortArrayTo(pos: Int, ia: Array[Short]): Int

  def encodeCharArrayTo(pos: Int, ia: Array[Char]): Int

  def encodeIntArrayTo(pos: Int, ia: Array[Int]): Int

  def encodeLongArrayTo(pos: Int, ia: Array[Long]): Int

  def encodeBooleanArrayTo(pos: Int, ia: Array[Boolean]): Int

  def encodeFloatArrayTo(pos: Int, ia: Array[Float]): Int

  def encodeDoubleArrayTo(pos: Int, ia: Array[Double]): Int

  def copyTo(pos: Int, bytes: Array[Byte]): Int

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

  def encodeByteTo(pos: Int, value: Byte): Int = {
    arr(pos) = value
    pos + 1
  }

  def encodeByteAtEnd(pos: Int, value: Byte): Unit =
    arr(pos) = value

  def encodeShortAtEnd(pos: Int, value: Short): Unit =
    Util.encodeShortTo(arr, pos, value)

  def encodeCharAtEnd(pos: Int, value: Char): Unit =
    Util.encodeCharTo(arr, pos, value)

  def encodeIntAtEnd(pos: Int, value: Int): Unit =
    Util.encodeIntTo(arr, pos, value)

  def encodeLongAtEnd(pos: Int, value: Long): Unit =
    Util.encodeLongTo(arr, pos, value)

  def encodeIntTo(pos: Int, value: Int): Int =
    Util.encodeIntTo(arr, pos, value)

  def encodeStringTo(pos: Int, value: String): Int =
    Util.encodeStringTo(arr, pos, value)

  def encodeBooleanTo(pos: Int, value: Boolean): Int =
    Util.encodeBooleanTo(arr, pos, value)

  def encodeByteArrayTo(pos: Int, ia: Array[Byte]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, arr, destOffset + newpos, ia.length * 1)
    newpos + ia.length * 1
  }

  def encodeShortArrayTo(pos: Int, ia: Array[Short]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val srcOffset = UnsafeMemory.shortArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, arr, destOffset + newpos, ia.length * 2)
    newpos + ia.length * 2
  }

  def encodeCharArrayTo(pos: Int, ia: Array[Char]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val srcOffset = UnsafeMemory.charArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, arr, destOffset + newpos, ia.length * 4)
    newpos + ia.length * 4
  }

  def encodeIntArrayTo(pos: Int, ia: Array[Int]): Int = {
    // no bounds check

    // 1. store length of array
    val newpos = encodeIntTo(pos, ia.length)

    // 2. copy memory
    val srcOffset = UnsafeMemory.intArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    // copy ia into arr
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, arr, destOffset + newpos, ia.length * 4)

    newpos + ia.length * 4
  }

  def encodeLongArrayTo(pos: Int, ia: Array[Long]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val srcOffset = UnsafeMemory.longArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, arr, destOffset + newpos, ia.length * 8)
    newpos + ia.length * 8
  }

  def encodeBooleanArrayTo(pos: Int, ia: Array[Boolean]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val srcOffset = UnsafeMemory.booleanArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, arr, destOffset + newpos, ia.length * 1)
    newpos + ia.length * 1
  }

  def encodeFloatArrayTo(pos: Int, ia: Array[Float]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val srcOffset = UnsafeMemory.floatArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, arr, destOffset + newpos, ia.length * 4)
    newpos + ia.length * 4
  }

  def encodeDoubleArrayTo(pos: Int, ia: Array[Double]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val srcOffset = UnsafeMemory.doubleArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, arr, destOffset + newpos, ia.length * 8)
    newpos + ia.length * 8
  }

  def copyTo(pos: Int, bytes: Array[Byte]): Int = {
    Util.copy(arr, pos, bytes)
    pos + bytes.length
  }

  def decodeByteFrom(pos: Int): Byte = {
    arr(pos)
  }

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

  def decodeByteArrayFrom(pos: Int): (Array[Byte], Int) = {
    val len = decodeIntFrom(pos)
    val nextPos = pos+4
    val ia = Array.ofDim[Byte](len)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 1)
    (ia, nextPos + len * 1)
  }

  def decodeShortArrayFrom(pos: Int): (Array[Short], Int) = {
    val len = decodeIntFrom(pos)
    val nextPos = pos+4
    val ia = Array.ofDim[Short](len)
    val srcOffset = UnsafeMemory.shortArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 2)
    (ia, nextPos + len * 2)
  }

  def decodeCharArrayFrom(pos: Int): (Array[Char], Int) = {
    val len = decodeIntFrom(pos)
    val nextPos = pos+4
    val ia = Array.ofDim[Char](len)
    val srcOffset = UnsafeMemory.charArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 4)
    (ia, nextPos + len * 4)
  }

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int) = {
    // arr: Array[Byte]
    // 1. read length
    val len = decodeIntFrom(pos)
    val nextPos = pos+4

    // 2. allocate Array[Int] TODO: use Unsafe
    val ia = Array.ofDim[Int](len)

    // 3. the copy
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 4)

    (ia, nextPos + len * 4)
  }

  def decodeLongArrayFrom(pos: Int): (Array[Long], Int) = {
    val len = decodeIntFrom(pos)
    val nextPos = pos+4
    val ia = Array.ofDim[Long](len)
    val srcOffset = UnsafeMemory.longArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 8)
    (ia, nextPos + len * 8)
  }

  def decodeBooleanArrayFrom(pos: Int): (Array[Boolean], Int) = {
    val len = decodeIntFrom(pos)
    val nextPos = pos+4
    val ia = Array.ofDim[Boolean](len)
    val srcOffset = UnsafeMemory.booleanArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 1)
    (ia, nextPos + len * 1)
  }

  def decodeFloatArrayFrom(pos: Int): (Array[Float], Int) = {
    val len = decodeIntFrom(pos)
    val nextPos = pos+4
    val ia = Array.ofDim[Float](len)
    val srcOffset = UnsafeMemory.floatArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 4)
    (ia, nextPos + len * 4)
  }

  def decodeDoubleArrayFrom(pos: Int): (Array[Double], Int) = {
    val len = decodeIntFrom(pos)
    val nextPos = pos+4
    val ia = Array.ofDim[Double](len)
    val srcOffset = UnsafeMemory.doubleArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 8)
    (ia, nextPos + len * 8)
  }

  def toArray: Array[Byte] =
    arr
}

/* Implementation of `ByteBuffer` using `ArrayBuffer`.
 */
final class ByteArrayBuffer extends ByteBuffer {

  // private val buf = ArrayBuffer[Byte]()
  private val buf = new ArrayBuilder.ofByte()

  def encodeByteTo(pos: Int, value: Byte): Int = {
    buf += value
    pos + 1
  }

  // pos is ignored!
  def encodeByteAtEnd(pos: Int, value: Byte): Unit = {
    ???
  }

  // pos is ignored!
  def encodeShortAtEnd(pos: Int, value: Short): Unit = {
    ???
  }

  // pos is ignored!
  def encodeCharAtEnd(pos: Int, value: Char): Unit = {
    ???
  }

  // pos is ignored!
  def encodeIntAtEnd(pos: Int, value: Int): Unit = {
    // assert(buf.size == pos)
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
  def encodeLongAtEnd(pos: Int, value: Long): Unit = {
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

  def encodeIntTo(pos: Int, value: Int): Int = {
    Util.encodeIntTo(buf, pos, value)
    pos + 4
  }

  def encodeStringTo(pos: Int, value: String): Int = {
    ???
  }

  def encodeBooleanTo(pos: Int, value: Boolean): Int = {
    ???
  }

  def encodeByteArrayTo(pos: Int, ia: Array[Byte]): Int = {
    ???
  }

  def encodeShortArrayTo(pos: Int, ia: Array[Short]): Int = {
    ???
  }

  def encodeCharArrayTo(pos: Int, ia: Array[Char]): Int = {
    ???
  }

  def encodeIntArrayTo(pos: Int, ia: Array[Int]): Int = {
    ???
  }

  def encodeLongArrayTo(pos: Int, ia: Array[Long]): Int = {
    ???
  }

  def encodeBooleanArrayTo(pos: Int, ia: Array[Boolean]): Int = {
    ???
  }

  def encodeFloatArrayTo(pos: Int, ia: Array[Float]): Int = {
    ???
  }

  def encodeDoubleArrayTo(pos: Int, ia: Array[Double]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val ba = Array.ofDim[Byte](ia.length * 8)
    val srcOffset = UnsafeMemory.doubleArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, ba, destOffset, ia.length * 8)
    buf ++= ba
    newpos + ia.length * 8
  }

  def copyTo(pos: Int, bytes: Array[Byte]): Int = {
    // assume buf.size = buf
    buf ++= bytes
    pos + bytes.length
  }

  def decodeByteFrom(pos: Int): Byte = {
    ???
  }

  def decodeShortFrom(pos: Int): Short = {
    ???
  }

  def decodeCharFrom(pos: Int): Char = {
    ???
  }

  def decodeIntFrom(pos: Int): Int = {
    ???
  }

  def decodeLongFrom(pos: Int): Long = {
    ???
  }

  def decodeStringFrom(pos: Int): (String, Int) = {
    ???
  }

  def decodeBooleanFrom(pos: Int): Boolean = {
    ???
  }

  def decodeByteArrayFrom(pos: Int): (Array[Byte], Int) = {
    ???
  }

  def decodeShortArrayFrom(pos: Int): (Array[Short], Int) = {
    ???
  }

  def decodeCharArrayFrom(pos: Int): (Array[Char], Int) = {
    ???
  }

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int) = {
    ???
  }

  def decodeLongArrayFrom(pos: Int): (Array[Long], Int) = {
    ???
  }

  def decodeBooleanArrayFrom(pos: Int): (Array[Boolean], Int) = {
    ???
  }

  def decodeFloatArrayFrom(pos: Int): (Array[Float], Int) = {
    ???
  }

  def decodeDoubleArrayFrom(pos: Int): (Array[Double], Int) = {
    ???
  }

  def toArray: Array[Byte] =
    buf.result
}
