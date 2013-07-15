package scala.pickling
package io

import scala.pickling.binary._

class ByteBufferOutput(buf: java.nio.ByteBuffer) extends EncodingOutput[Array[Byte]] {

  require(buf.limit() == buf.capacity(), "buffer limit too small")

  def result(): Array[Byte] = {
    buf.flip()
    val bytes = Array.ofDim[Byte](buf.remaining)
    buf.get(bytes)
    bytes
  }

  def put(obj: Array[Byte]): this.type = {
    buf.put(obj)
  	this
  }

  def encodeByteTo(pos: Int, value: Byte): Int = {
    val endPos = pos + 1
    if (buf.position() < endPos)
      buf.position(endPos)
    buf.put(pos, value)
    endPos
  }

  // TODO: can we ensure that the conditional is unnecessary?
  // i.e., it would be great if the implementation of PickleBuilder could ensure that
  // buf.position() >= pos is always the case
  def encodeByteAtEnd(pos: Int, value: Byte): Unit = {
    if (buf.position() < pos)
      buf.position(pos)
    buf.put(value)
  }

  def encodeShortAtEnd(pos: Int, value: Short): Unit = {
    if (buf.position() < pos)
      buf.position(pos)
    buf.putShort(value)
  }

  def encodeCharAtEnd(pos: Int, value: Char): Unit = {
    if (buf.position() < pos)
      buf.position(pos)
    buf.putChar(value)
  }

  def encodeIntAtEnd(pos: Int, value: Int): Unit = {
    if (buf.position() < pos)
      buf.position(pos)
    buf.putInt(value)
  }

  def encodeLongAtEnd(pos: Int, value: Long): Unit = {
    if (buf.position() < pos)
      buf.position(pos)
    buf.putLong(value)
  }

  def encodeIntTo(pos: Int, value: Int): Int = {
    val endPos = pos + 4
    if (buf.position() < endPos)
      buf.position(endPos)
    buf.putInt(pos, value)
    endPos
  }

  def encodeStringTo(pos: Int, value: String): Int = {
    val bytes = value.getBytes("UTF-8")
    encodeIntTo(pos, bytes.length) // requires 4 bytes
    copyTo(pos + 4, bytes)
    pos + 4 + bytes.length
  }

  def encodeBooleanTo(pos: Int, value: Boolean): Int = {
    encodeByteTo(pos, if (value) 1 else 0)
  }

  def encodeIntArrayTo(pos: Int, ia: Array[Int]): Int = {
    // no bounds check

    // 1. store length of array
    val newpos = encodeIntTo(pos, ia.length)

    // 2. allocate temp byte array
    val ba = Array.ofDim[Byte](ia.length * 4)

    // 3. copy memory
    val srcOffset = UnsafeMemory.intArrayOffset
    val destOffset = UnsafeMemory.byteArrayOffset

    // copy ia into ba
    UnsafeMemory.unsafe.copyMemory(ia, srcOffset, ba, destOffset, ia.length * 4)

    // 4. concatenate temp byte array to buf
    copyTo(pos + 4, ba)

    newpos + ia.length * 4
  }

  def copyTo(pos: Int, bytes: Array[Byte]): Int = {
    buf.position(pos)
    buf.put(bytes)
    pos + bytes.length
  }
}

class ByteBufferInput(buf: java.nio.ByteBuffer) extends DecodingInput {

  def decodeByteFrom(pos: Int): (Byte, Int) = {
    (buf.get(pos), pos + 1)
  }

  def decodeShortFrom(pos: Int): (Short, Int) = {
    (buf.getShort(pos), pos + 2)
  }

  def decodeCharFrom(pos: Int): (Char, Int) = {
    (buf.getChar(pos), pos + 2)
  }

  def decodeIntFrom(pos: Int): (Int, Int) = {
    (buf.getInt(pos), pos + 4)
  }

  def decodeLongFrom(pos: Int): (Long, Int) = {
    (buf.getLong(pos), pos + 8)
  }

  def decodeStringFrom(pos: Int): (String, Int) = {
    val len = buf.getInt(pos)
    val bytes = Array.ofDim[Byte](len)
    var i = 0
    while (i < len) {
      bytes(i) = buf.get(pos + 4 + i)
      i += 1
    }
    (new String(bytes, "UTF-8"), pos + 4 + len)
  }

  def decodeBooleanFrom(pos: Int): (Boolean, Int) = {
    (buf.get(pos) != 0, pos + 1)
  }

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int) = {
    // 1. read length
    val (len, nextPos) = decodeIntFrom(pos)

    // 2. allocate Array[Int] TODO: use Unsafe
    val ia = Array.ofDim[Int](len)

    // 3. the copy
    var i = 0
    while (i < len) {
      ia(i) = buf.getInt(nextPos + (i * 4))
      i += 1
    }

    (ia, nextPos + (len * 4))
  }

}
