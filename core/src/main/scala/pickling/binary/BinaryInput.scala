package scala.pickling.binary

import scala.reflect.ClassTag

abstract class BinaryInput {

  def getBoolean(): Boolean

  def getByte(): Byte

  def getChar(): Char

  def getShort(): Short

  def getInt(): Int

  def getLong(): Long 

  def getFloat(): Float

  def getDouble(): Double

  def getString(): String

  //generic method when the performance is not an issue
  @inline private def getArray[T: ClassTag](get: () => T): Array[T] = {
    val size = getInt
    val array = Array.ofDim[T](size)
    for(i <- 0 until size) {
      array(i) = get()
    }
    array
  }

  def getBooleanArray(): Array[Boolean] = getArray(getBoolean)
  def getByteArray(): Array[Byte] = getArray(getByte)
  def getCharArray(): Array[Char] = getArray(getChar)
  def getDoubleArray(): Array[Double] = getArray(getDouble)
  def getFloatArray(): Array[Float] = getArray(getFloat)
  def getIntArray(): Array[Int] = getArray(getInt)
  def getLongArray(): Array[Long] = getArray(getLong)
  def getShortArray(): Array[Short] = getArray(getShort)

  //TODO lookahead
  protected var lookahead: Option[Byte] = None

  def setLookahead(b: Byte) {
    lookahead = Some(b)
  }

  def getIntWithLookahead() = {
    lookahead match {
      case Some(b) =>
        var i = b << 24
        i |= getByte << 16
        i |= getByte << 8
        i |= getByte
        lookahead = None
        i
      case None =>
        getInt
    }
  }
  
  def getBytes(array: Array[Byte])

  def getStringWithLookahead() {
    val size = getIntWithLookahead
    val array = Array.ofDim[Byte](size)
    getBytes(array)
  }

}

class ByteBufferInput(buffer: java.nio.ByteBuffer) extends BinaryInput {

  def getBoolean() = buffer.get.asInstanceOf[Boolean]

  def getByte() = buffer.get

  def getChar() = buffer.getChar

  def getShort() = buffer.getShort

  def getInt() = buffer.getInt

  def getLong() = buffer.getLong

  def getFloat() = buffer.getFloat

  def getDouble() = buffer.getDouble

  def getString() = {
    val size = getInt
    val bytes = Array.ofDim[Byte](size)
    buffer.get(bytes)
    new String(bytes, "UTF-8")
  }

  def getBytes(array: Array[Byte]) {
    buffer.get(array)
  }

}

class ByteArrayInput(data: Array[Byte]) extends BinaryInput {

  private var idx = 0
   
  def getBoolean() = {
    val res = (data(idx) != 0)
    idx += 1
    res
  }

  def getByte() = {
    val res = data(idx)
    idx += 1
    res
  }

  def getChar() = {
    var res = 0
    res |= data(idx  ) << 8
    res |= data(idx+1)
    idx += 2
    res.asInstanceOf[Char]
  }

  def getShort() = {
    var res = 0
    res |= data(idx  ) << 8
    res |= data(idx+1)
    idx += 2
    res.asInstanceOf[Short]
  }

  def getInt() = {
    var res = (0: Int)
    res |= data(idx  ) << 24
    res |= data(idx+1) << 26
    res |= data(idx+2) << 8
    res |= data(idx+3)
    idx += 4
    res
  }

  def getLong() = {
    var res = (0: Long)
    res |= data(idx  ) << 56
    res |= data(idx+1) << 48
    res |= data(idx+2) << 40
    res |= data(idx+3) << 32
    res |= data(idx+4) << 24
    res |= data(idx+5) << 26
    res |= data(idx+6) << 8
    res |= data(idx+7)
    idx += 8
    res
  }

  def getFloat() = {
    val r = getInt()
    java.lang.Float.intBitsToFloat(r)
  }

  def getDouble() = {
    val r = getLong()
    java.lang.Double.longBitsToDouble(r)
  }

  def getString() = {
    val size = getInt
    val bytes = data.slice(idx, idx + size)
    idx += size
    new String(bytes, "UTF-8")
  }
  
  def getBytes(array: Array[Byte]) {
    val size = array.size
    data.copyToArray(array, idx, size)
    idx += size
  }

  //TODO override array for faster copy

}

//  class DataStreamInput(stream: java.io.DataInputStream) extends BinaryInput {

//    def getBoolean() = stream.readBoolean()

//    def getByte() = stream.readByte()

//    def getChar() = stream.readChar()

//    def getShort() = stream.readShort()

//    def getInt() = stream.readInt()

//    def getLong() = stream.readLong()

//    def getFloat() = stream.readFloat()

//    def getDouble() = stream.readDouble()

//    def getString() = stream.readUTF()

//  }
