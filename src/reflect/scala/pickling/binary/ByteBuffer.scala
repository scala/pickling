package scala.pickling.binary

import scala.collection.mutable.ArrayBuffer

sealed abstract class ByteBuffer {
  def encodeByteTo(pos: Int, value: Byte): Int

  def encodeIntAtEnd(pos: Int, value: Int): Unit

  def encodeIntTo(pos: Int, value: Int): Int

  def encodeStringTo(pos: Int, value: String): Int

  def encodeBooleanTo(pos: Int, value: Boolean): Int

  def encodeIntArrayTo(pos: Int, ia: Array[Int]): Int

  def copyTo(pos: Int, bytes: Array[Byte]): Int

  def decodeByteFrom(pos: Int): (Byte, Int)

  def decodeIntFrom(pos: Int): (Int, Int)

  def decodeStringFrom(pos: Int): (String, Int)

  def decodeBooleanFrom(pos: Int): (Boolean, Int)

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int)

  def toArray: Array[Byte]
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

  def encodeIntAtEnd(pos: Int, value: Int): Unit =
    Util.encodeIntTo(arr, pos, value)

  def encodeIntTo(pos: Int, value: Int): Int =
    Util.encodeIntTo(arr, pos, value)

  def encodeStringTo(pos: Int, value: String): Int =
    Util.encodeStringTo(arr, pos, value)

  def encodeBooleanTo(pos: Int, value: Boolean): Int =
    Util.encodeBooleanTo(arr, pos, value)

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

  def copyTo(pos: Int, bytes: Array[Byte]): Int = {
    Util.copy(arr, pos, bytes)
    pos + bytes.length
  }

  def decodeByteFrom(pos: Int): (Byte, Int) = {
    (arr(pos), pos + 1)
  }

  def decodeIntFrom(pos: Int): (Int, Int) =
    Util.decodeIntFrom(arr, pos)

  def decodeStringFrom(pos: Int): (String, Int) =
    Util.decodeStringFrom(arr, pos)

  def decodeBooleanFrom(pos: Int): (Boolean, Int) =
    Util.decodeBooleanFrom(arr, pos)

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int) = {
    // arr: Array[Byte]
    // 1. read length
    val (len, nextPos) = decodeIntFrom(pos)

    // 2. allocate Array[Int] TODO: use Unsafe
    val ia = Array.ofDim[Int](len)

    // 3. the copy
    val srcOffset = UnsafeMemory.byteArrayOffset
    val destOffset = UnsafeMemory.intArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len*4)

    (ia, nextPos + len*4)
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

  // pos is ingored!
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

  def copyTo(pos: Int, bytes: Array[Byte]): Int = {
    // assume buf.size = buf
    buf ++= bytes
    pos + bytes.length
  }

  def decodeByteFrom(pos: Int): (Byte, Int) = {
    (buf(pos), pos+1)
  }

  def decodeIntFrom(pos: Int): (Int, Int) = {
    val fst = (buf(pos) << 24).toInt
    val snd = ((buf(pos+1) << 16) & 0x00FFFFFF).toInt
    val thrd = ((buf(pos+2) << 8) & 0x0000FFFF).toInt
    val frth = (buf(pos+3) & 0x000000FF).toInt
    (fst | snd | thrd | frth, pos+4)
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
    val newbuf = buf.slice(nextPos, nextPos + (len*4))
    val ba: Array[Byte] = newbuf.toArray
    UnsafeMemory.unsafe.copyMemory(ba, srcOffset, ia, destOffset, len*4)

    (ia, nextPos + (len*4))
  }

  def toArray: Array[Byte] =
    buf.toArray
}
