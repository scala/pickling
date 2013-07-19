package scala.pickling.binary

import scala.collection.mutable.ArrayBuffer
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

  def decodeByteFrom(pos: Int): (Byte, Int)

  def decodeShortFrom(pos: Int): (Short, Int)

  def decodeCharFrom(pos: Int): (Char, Int)

  def decodeIntFrom(pos: Int): (Int, Int)

  def decodeLongFrom(pos: Int): (Long, Int)

  def decodeStringFrom(pos: Int): (String, Int)

  def decodeBooleanFrom(pos: Int): (Boolean, Int)

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

  def decodeByteFrom(pos: Int): (Byte, Int) = {
    (arr(pos), pos + 1)
  }

  def decodeShortFrom(pos: Int): (Short, Int) =
    Util.decodeShortFrom(arr, pos)

  def decodeCharFrom(pos: Int): (Char, Int) =
    Util.decodeCharFrom(arr, pos)

  def decodeIntFrom(pos: Int): (Int, Int) =
    Util.decodeIntFrom(arr, pos)

  def decodeLongFrom(pos: Int): (Long, Int) =
    Util.decodeLongFrom(arr, pos)

  def decodeStringFrom(pos: Int): (String, Int) =
    Util.decodeStringFrom(arr, pos)

  def decodeBooleanFrom(pos: Int): (Boolean, Int) =
    Util.decodeBooleanFrom(arr, pos)

  def decodeByteArrayFrom(pos: Int): (Array[Byte], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Byte](len)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 1)
    (ia, nextPos + len * 1)
  }

  def decodeShortArrayFrom(pos: Int): (Array[Short], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Short](len)
    val srcOffset = UnsafeMemory.shortArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 2)
    (ia, nextPos + len * 2)
  }

  def decodeCharArrayFrom(pos: Int): (Array[Char], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Char](len)
    val srcOffset = UnsafeMemory.charArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 4)
    (ia, nextPos + len * 4)
  }

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int) = {
    // arr: Array[Byte]
    // 1. read length
    val (len, nextPos) = decodeIntFrom(pos)

    // 2. allocate Array[Int] TODO: use Unsafe
    val ia = Array.ofDim[Int](len)

    // 3. the copy
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 4)

    (ia, nextPos + len * 4)
  }

  def decodeLongArrayFrom(pos: Int): (Array[Long], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Long](len)
    val srcOffset = UnsafeMemory.longArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 8)
    (ia, nextPos + len * 8)
  }

  def decodeBooleanArrayFrom(pos: Int): (Array[Boolean], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Boolean](len)
    val srcOffset = UnsafeMemory.booleanArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 1)
    (ia, nextPos + len * 1)
  }

  def decodeFloatArrayFrom(pos: Int): (Array[Float], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Float](len)
    val srcOffset = UnsafeMemory.floatArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 4)
    (ia, nextPos + len * 4)
  }

  def decodeDoubleArrayFrom(pos: Int): (Array[Double], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
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

  private val buf = ArrayBuffer[Byte]()

  def encodeByteTo(pos: Int, value: Byte): Int = {
    if (buf.size < pos + 1) {
      val missing = pos + 1 - buf.size
      buf ++= Array.ofDim[Byte](missing)
    }
    buf(pos) = value
    pos + 1
  }

  // pos is ignored!
  def encodeByteAtEnd(pos: Int, value: Byte): Unit = {
    buf += value
  }

  // pos is ignored!
  def encodeShortAtEnd(pos: Int, value: Short): Unit = {
    val fst = (value >>> 8 & 0xff).asInstanceOf[Byte]
    val snd = (value & 0xff).asInstanceOf[Byte]
    buf += fst
    buf += snd
  }

  // pos is ignored!
  def encodeCharAtEnd(pos: Int, value: Char): Unit = {
    val fst = (value >>> 8 & 0xff).asInstanceOf[Byte]
    val snd = (value & 0xff).asInstanceOf[Byte]
    buf += fst
    buf += snd
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
    if (buf.size < pos + 4) {
      val missing = pos + 4 - buf.size
      buf ++= Array.ofDim[Byte](missing)
    }
    Util.encodeIntTo(buf, pos, value)
    pos + 4
  }

  def encodeStringTo(pos: Int, value: String): Int = {
    // assume pos == buf.size
    val bytes = value.getBytes("UTF-8")
    val len   = bytes.length
    val next  = encodeIntTo(pos, len)
    buf ++= bytes
    next + len
  }

  def encodeBooleanTo(pos: Int, value: Boolean): Int = {
    if (buf.size <= pos) buf += 0
    buf(pos) = if (value) 1 else 0
    pos + 1
  }

  def encodeByteArrayTo(pos: Int, ia: Array[Byte]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val ba = Array.ofDim[Byte](ia.length * 1)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, ba, destOffset, ia.length * 1)
    buf ++= ba
    newpos + ia.length * 1
  }

  def encodeShortArrayTo(pos: Int, ia: Array[Short]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val ba = Array.ofDim[Byte](ia.length * 2)
    val srcOffset = UnsafeMemory.shortArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, ba, destOffset, ia.length * 2)
    buf ++= ba
    newpos + ia.length * 2
  }

  def encodeCharArrayTo(pos: Int, ia: Array[Char]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val ba = Array.ofDim[Byte](ia.length * 4)
    val srcOffset = UnsafeMemory.charArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, ba, destOffset, ia.length * 4)
    buf ++= ba
    newpos + ia.length * 4
  }

  def encodeIntArrayTo(pos: Int, ia: Array[Int]): Int = {
    // no bounds check

    // 1. store length of array
    val newpos = encodeIntTo(pos, ia.length)

    // 2. allocate temp byte array
    val ba = Array.ofDim[Byte](ia.length * 4)

    // 2. copy memory
    val srcOffset = UnsafeMemory.intArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset

    // copy ia into ba
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, ba, destOffset, ia.length * 4)

    // 3. concatenate temp byte array to buf
    buf ++= ba

    newpos + ia.length * 4
  }

  def encodeLongArrayTo(pos: Int, ia: Array[Long]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val ba = Array.ofDim[Byte](ia.length * 8)
    val srcOffset = UnsafeMemory.charArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, ba, destOffset, ia.length * 8)
    buf ++= ba
    newpos + ia.length * 8
  }

  def encodeBooleanArrayTo(pos: Int, ia: Array[Boolean]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val ba = Array.ofDim[Byte](ia.length * 1)
    val srcOffset = UnsafeMemory.booleanArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, ba, destOffset, ia.length * 1)
    buf ++= ba
    newpos + ia.length * 1
  }

  def encodeFloatArrayTo(pos: Int, ia: Array[Float]): Int = {
    val newpos = encodeIntTo(pos, ia.length)
    val ba = Array.ofDim[Byte](ia.length * 4)
    val srcOffset = UnsafeMemory.floatArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, ba, destOffset, ia.length * 4)
    buf ++= ba
    newpos + ia.length * 4
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

  def decodeByteFrom(pos: Int): (Byte, Int) = {
    (buf(pos), pos+1)
  }

  def decodeShortFrom(pos: Int): (Short, Int) = {
    val fst = ((buf(pos) << 8) & 0xFFFF).toShort
    val snd = (buf(pos+1)      & 0x00FF).toShort
    ((fst | snd).toShort, pos+2)
  }

  def decodeCharFrom(pos: Int): (Char, Int) = {
    val fst = ((buf(pos) << 8) & 0xFFFF).toChar
    val snd = (buf(pos+1)      & 0x00FF).toChar
    ((fst | snd).toChar, pos+2)
  }

  def decodeIntFrom(pos: Int): (Int, Int) = {
    val fst = (buf(pos) << 24).toInt
    val snd = ((buf(pos+1) << 16) & 0x00FFFFFF).toInt
    val thrd = ((buf(pos+2) << 8) & 0x0000FFFF).toInt
    val frth = (buf(pos+3) & 0x000000FF).toInt
    (fst | snd | thrd | frth, pos+4)
  }

  def decodeLongFrom(pos: Int): (Long, Int) = {
    val elem1 = ((buf(pos) << 56)   & 0xFFFFFFFFFFFFFFFFL).toLong
    val elem2 = ((buf(pos+1) << 48) & 0xFFFFFFFFFFFFFFL).toLong
    val elem3 = ((buf(pos+2) << 40) & 0xFFFFFFFFFFFFL).toLong
    val elem4 = ((buf(pos+3) << 32) & 0xFFFFFFFFFFL).toLong
    val elem5 = ((buf(pos+4) << 24) & 0xFFFFFFFF).toLong
    val elem6 = ((buf(pos+5) << 16) & 0x00FFFFFF).toLong
    val elem7 = ((buf(pos+6) << 8) & 0x0000FFFF).toLong
    val elem8 = (buf(pos+7) & 0x000000FF).toLong
    (elem1 | elem2 | elem3 | elem4 | elem5 | elem6 | elem7 | elem8, pos+8)
  }

  def decodeStringFrom(pos: Int): (String, Int) = {
    val (len, next) = decodeIntFrom(pos)
    println(s"decoding string of length $len, starting from $next")
    val bytes = buf.slice(pos + 4, pos + 4 + len).toArray
    println("bytes: " + bytes.mkString(","))
    val res = new String(bytes, "UTF-8")
    println(s"result string: $res")
    (res, pos + 4 + len)
  }

  def decodeBooleanFrom(pos: Int): (Boolean, Int) = {
    val (value, next) = decodeIntFrom(pos)
    (value != 0, next)
  }

  def decodeByteArrayFrom(pos: Int): (Array[Byte], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Byte](len)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    val newbuf = buf.slice(nextPos, nextPos + len * 1)
    val ba: Array[Byte] = newbuf.toArray
    UnsafeMemory.unsafe.copyMemory(ba, srcOffset, ia, destOffset, len * 1)
    (ia, nextPos + len * 1)
  }

  def decodeShortArrayFrom(pos: Int): (Array[Short], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Short](len)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    val newbuf = buf.slice(nextPos, nextPos + len * 2)
    val ba: Array[Byte] = newbuf.toArray
    UnsafeMemory.unsafe.copyMemory(ba, srcOffset, ia, destOffset, len * 2)
    (ia, nextPos + len * 2)
  }

  def decodeCharArrayFrom(pos: Int): (Array[Char], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Char](len)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    val newbuf = buf.slice(nextPos, nextPos + len * 4)
    val ba: Array[Byte] = newbuf.toArray
    UnsafeMemory.unsafe.copyMemory(ba, srcOffset, ia, destOffset, len * 4)
    (ia, nextPos + len * 4)
  }

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int) = {
    // buf: ArrayBuffer[Byte]
    // 1. read length
    val (len, nextPos) = decodeIntFrom(pos)

    // 2. allocate Array[Int] TODO: use Unsafe
    val ia = Array.ofDim[Int](len)

    // 3. the copy
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    // read the required num of bytes from `buf`
    val newbuf = buf.slice(nextPos, nextPos + len * 4)
    val ba: Array[Byte] = newbuf.toArray
    UnsafeMemory.unsafe.copyMemory(ba, srcOffset, ia, destOffset, len * 4)

    (ia, nextPos + len * 4)
  }

  def decodeLongArrayFrom(pos: Int): (Array[Long], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Long](len)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    val newbuf = buf.slice(nextPos, nextPos + len * 8)
    val ba: Array[Byte] = newbuf.toArray
    UnsafeMemory.unsafe.copyMemory(ba, srcOffset, ia, destOffset, len * 8)
    (ia, nextPos + len * 8)
  }

  def decodeBooleanArrayFrom(pos: Int): (Array[Boolean], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Boolean](len)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    val newbuf = buf.slice(nextPos, nextPos + len * 1)
    val ba: Array[Byte] = newbuf.toArray
    UnsafeMemory.unsafe.copyMemory(ba, srcOffset, ia, destOffset, len * 1)
    (ia, nextPos + len * 1)
  }

  def decodeFloatArrayFrom(pos: Int): (Array[Float], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Float](len)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    val newbuf = buf.slice(nextPos, nextPos + len * 4)
    val ba: Array[Byte] = newbuf.toArray
    UnsafeMemory.unsafe.copyMemory(ba, srcOffset, ia, destOffset, len * 4)
    (ia, nextPos + len * 4)
  }

  def decodeDoubleArrayFrom(pos: Int): (Array[Double], Int) = {
    val (len, nextPos) = decodeIntFrom(pos)
    val ia = Array.ofDim[Double](len)
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    val newbuf = buf.slice(nextPos, nextPos + len * 8)
    val ba: Array[Byte] = newbuf.toArray
    UnsafeMemory.unsafe.copyMemory(ba, srcOffset, ia, destOffset, len * 8)
    (ia, nextPos + len * 8)
  }

  def toArray: Array[Byte] =
    buf.toArray
}
