package scala.pickling.binary

import scala.reflect.ClassTag

abstract class BinaryInput {

  def getBoolean(): Boolean = {
    getByte() != 0 
  }

  def getByte(): Byte

  def getChar(): Char

  def getShort(): Short

  def getInt(): Int

  def getLong(): Long 

  def getFloat(): Float

  def getDouble(): Double

  def getString(): String = {
    val array = getByteArray
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
        i |= (getByte.toInt << 16) & 0xFF0000
        i |= (getByte.toInt << 8) & 0xFF00
        i |= (getByte.toInt) & 0xFF
        lookahead = None
        i
      case None =>
        getInt
    }
  }
  
  def getStringWithLookahead(la: Byte): String = {
    val oldLa = lookahead
    setLookahead(la)
    val res = getString
    lookahead = oldLa
    res
  }

}

class ByteBufferInput(buffer: java.nio.ByteBuffer) extends BinaryInput {

  import java.nio.ByteOrder
  assert(buffer.order == ByteOrder.BIG_ENDIAN)

  def getByte() = buffer.get

  def getChar() = buffer.getChar

  def getShort() = buffer.getShort

  def getInt() = buffer.getInt

  def getLong() = buffer.getLong

  def getFloat() = buffer.getFloat

  def getDouble() = buffer.getDouble

  override def getByteArray(): Array[Byte] = {
    val size = getIntWithLookahead
    val array = Array.ofDim[Byte](size)
    buffer.get(array)
    array
  }

}

class ByteArrayInput(data: Array[Byte]) extends BinaryInput {

  private var idx = 0
   
  def getByte() = {
    val res = data(idx)
    idx += 1
    res
  }

  def getChar() = {
    var res = 0
    res |= data(idx  ) << 8
    res |= data(idx+1).toInt & 0xFF
    idx += 2
    res.asInstanceOf[Char]
  }

  def getShort() = {
    var res = 0
    res |= data(idx  ) << 8
    res |= data(idx+1).toInt & 0xFF
    idx += 2
    res.asInstanceOf[Short]
  }

  def getInt() = {
    var res = (0: Int)
    res |= (data(idx  ) << 24)
    res |= (data(idx+1) << 16) & 0xFF0000
    res |= (data(idx+2) << 8 ) & 0xFF00
    res |= (data(idx+3)      ) & 0xFF
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

  override def getByteArray(): Array[Byte] = {
    val size = getIntWithLookahead
    val array = Array.ofDim[Byte](size)
    data.view(idx, idx+size).copyToArray(array, 0, size)
    idx += size
    array
  }

  //TODO override array for faster copy

}

class StreamInput(stream: java.io.InputStream) extends BinaryInput {
  val ds = new java.io.DataInputStream(stream)
  def getByte() = ds.readByte()
  def getChar() = ds.readChar()
  def getShort() = ds.readShort()
  def getInt() = ds.readInt()
  def getLong() = ds.readLong()
  def getFloat() = ds.readFloat()
  def getDouble() = ds.readDouble()

  override def getByteArray(): Array[Byte] = {
    val size = getIntWithLookahead
    val array = Array.ofDim[Byte](size)
    ds.readFully(array)
    array
  }

  //TODO check endianness
  
}
