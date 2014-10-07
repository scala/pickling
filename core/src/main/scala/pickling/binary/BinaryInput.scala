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

  def getString(): String = {
    val size = getIntWithLookahead
    val array = Array.ofDim[Byte](size)
    getBytes(array)
    new String(array, "UTF-8")
  }

  //generic method when the performance is not an issue
  @inline private def getArray[T: ClassTag](get: () => T): Array[T] = {
    val size = getIntWithLookahead
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

  def getStringWithLookahead(la: Byte): String = {
    val oldLa = lookahead
    setLookahead(la)
    val res = getString
    lookahead = oldLa
    res
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

//def getString() = {
//  val size = getInt
//  val bytes = Array.ofDim[Byte](size)
//  buffer.get(bytes)
//  new String(bytes, "UTF-8")
//}

  def getBytes(array: Array[Byte]) {
    buffer.get(array)
  }

}

class ByteArrayInput(data: Array[Byte]) extends BinaryInput {

  private var idx = 0
   
  def getBoolean() = {
    getByte() != 0 
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
    res |= data(idx+1) << 16
    res |= data(idx+2) << 8
    res |= data(idx+3)
    idx += 4
    res
  }

  def getLong() = {
    var res = (0: Long)
    res |= (data(idx  ).toLong << 56) & 0xFFFFFFFFFFFFFFFFL
    res |= (data(idx+1).toLong << 48) & 0x00FFFFFFFFFFFFFFL
    res |= (data(idx+2).toLong << 40) & 0x0000FFFFFFFFFFFFL
    res |= (data(idx+3).toLong << 32) & 0x000000FFFFFFFFFFL
    res |= (data(idx+4).toLong << 24) & 0x00000000FFFFFFFFL
    res |= (data(idx+5).toLong << 16) & 0x0000000000FFFFFFL
    res |= (data(idx+6).toLong << 8 ) & 0x000000000000FFFFL
    res |= (data(idx+7).toLong      ) & 0x00000000000000FFL
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

//def getString() = {
//  val size = getInt
//  val bytes = data.slice(idx, idx + size)
//  idx += size
//  new String(bytes, "UTF-8")
//}
  
  def getBytes(array: Array[Byte]) {
    val size = array.size
    data.view(idx, idx+size).copyToArray(array, 0, size)
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
