package scala.pickling.binaryopt

import scala.collection.mutable.ArrayBuffer

sealed abstract class ByteBuffer {
  def encodeIntTo(pos: Int, value: Int): Int

  def encodeStringTo(pos: Int, value: String): Int

  def encodeBooleanTo(pos: Int, value: Boolean): Int

  def copyTo(pos: Int, bytes: Array[Byte]): Int

  def decodeIntFrom(pos: Int): (Int, Int)

  def decodeStringFrom(pos: Int): (String, Int)

  def decodeBooleanFrom(pos: Int): (Boolean, Int)

  def toArray: Array[Byte]
}

/* Implementation of `ByteBuffer` using fixed-size array.
 */
final class ByteArray(arr: Array[Byte]) extends ByteBuffer {

  def this(size: Int) {
    this(Array.ofDim[Byte](size))
  }

  def encodeIntTo(pos: Int, value: Int): Int =
    Util.encodeIntTo(arr, pos, value)

  def encodeStringTo(pos: Int, value: String): Int =
    Util.encodeStringTo(arr, pos, value)

  def encodeBooleanTo(pos: Int, value: Boolean): Int =
    Util.encodeBooleanTo(arr, pos, value)

  def copyTo(pos: Int, bytes: Array[Byte]): Int = {
    Util.copy(arr, pos, bytes)
    pos + bytes.length
  }

  def decodeIntFrom(pos: Int): (Int, Int) =
    Util.decodeIntFrom(arr, pos)

  def decodeStringFrom(pos: Int): (String, Int) =
    Util.decodeStringFrom(arr, pos)

  def decodeBooleanFrom(pos: Int): (Boolean, Int) =
    Util.decodeBooleanFrom(arr, pos)

  def toArray: Array[Byte] =
    arr
}

/* Implementation of `ByteBuffer` using `ArrayBuffer`.
 */
final class ByteArrayBuffer extends ByteBuffer {

  private val buf = ArrayBuffer[Byte]()

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

  def copyTo(pos: Int, bytes: Array[Byte]): Int = {
    // assume buf.size = buf
    buf ++= bytes
    pos + bytes.length
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

  def toArray: Array[Byte] =
    buf.toArray
}
