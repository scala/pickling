package scala.pickling.binary

abstract class BinaryOutput {

  def putBoolean(value: Boolean): Unit

  def putByte(value: Byte): Unit

  def putChar(value: Char): Unit

  def putShort(value: Short): Unit

  def putInt(value: Int): Unit

  def putLong(value: Long): Unit

  def putFloat(value: Float): Unit

  def putDouble(value: Double): Unit

  def putString(value: String): Unit

  //generic method when the performance is not an issue
  @inline private def putArray[T](array: Array[T], put: T => Unit): Unit = {
    putInt(array.size)
    for(elt <- array) put(elt)
  }

  def putBooleanArray(value: Array[Boolean]): Unit = putArray(value, putBoolean)
  def putByteArray(value: Array[Byte]): Unit = putArray(value, putByte)
  def putCharArray(value: Array[Char]): Unit = putArray(value, putChar)
  def putDoubleArray(value: Array[Double]): Unit = putArray(value, putDouble)
  def putFloatArray(value: Array[Float]): Unit = putArray(value, putFloat)
  def putIntArray(value: Array[Int]): Unit = putArray(value, putInt)
  def putLongArray(value: Array[Long]): Unit = putArray(value, putLong)
  def putShortArray(value: Array[Short]): Unit = putArray(value, putShort)

}

//TODO how to deal with capacity issue ?
class ByteBufferOutput(buffer: java.nio.ByteBuffer) extends BinaryOutput {

  def putBoolean(value: Boolean) = buffer.put(value.asInstanceOf[Byte])

  def putByte(value: Byte) = buffer.put(value)

  def putChar(value: Char) = buffer.putChar(value)

  def putShort(value: Short) = buffer.putShort(value)

  def putInt(value: Int) = buffer.putInt(value)

  def putLong(value: Long) = buffer.putLong(value)

  def putFloat(value: Float) = buffer.putFloat(value)

  def putDouble(value: Double) = buffer.putDouble(value)

  def putString(value: String) {
    val bytes = value.getBytes("UTF-8")
    putInt(bytes.length)
    buffer.put(bytes)
  }

}

class ByteArrayOutput(initialCapacity: Int = 1024) extends BinaryOutput {

  val buffer = new java.io.ByteArrayOutputStream(initialCapacity)

  def putBoolean(value: Boolean) {
    if (value) buffer.write(1)
    else buffer.write(0)
  }

  def putByte(value: Byte) {
    buffer.write(value)
  }

  def putChar(value: Char) {
    val fst = value >>> 8 & 0xff
    val snd = value & 0xff
    buffer.write(fst)
    buffer.write(snd)
  }

  def putShort(value: Short) {
    val fst = value >>> 8 & 0xff
    val snd = value & 0xff
    buffer.write(fst)
    buffer.write(snd)
  }

  def putInt(value: Int) {
    val fst = value >>> 24
    val snd = value >>> 16 & 0xff
    val thrd = value >>> 8 & 0xff
    val frth = value & 0xff
    buffer.write(fst)
    buffer.write(snd)
    buffer.write(thrd)
    buffer.write(frth)
  }

  def putLong(value: Long) {
    val elem1 = (value >>> 56 & 0xff).asInstanceOf[Int]
    val elem2 = (value >>> 48 & 0xff).asInstanceOf[Int]
    val elem3 = (value >>> 40 & 0xff).asInstanceOf[Int]
    val elem4 = (value >>> 32 & 0xff).asInstanceOf[Int]
    val elem5 = (value >>> 24 & 0xff).asInstanceOf[Int]
    val elem6 = (value >>> 16 & 0xff).asInstanceOf[Int]
    val elem7 = (value >>> 8 & 0xff).asInstanceOf[Int]
    val elem8 = (value & 0xff).asInstanceOf[Int]
    buffer.write(elem1)
    buffer.write(elem2)
    buffer.write(elem3)
    buffer.write(elem4)
    buffer.write(elem5)
    buffer.write(elem6)
    buffer.write(elem7)
    buffer.write(elem8)
  }

  def putFloat(value: Float) {
    val intValue = java.lang.Float.floatToRawIntBits(value)
    putInt(intValue)
  }

  def putDouble(value: Double) {
    val longValue = java.lang.Double.doubleToRawLongBits(value)
    putLong(longValue)
  }

  def putString(value: String) {
    val bytes = value.getBytes("UTF-8")
    putInt(bytes.length)
    buffer.write(bytes)
  }

}

class DataStreamOutput(stream: java.io.DataOutputStream) extends BinaryOutput {
  
  def putBoolean(value: Boolean) = stream.writeBoolean(value)

  def putByte(value: Byte) = stream.writeByte(value)

  def putChar(value: Char) = stream.writeChar(value)

  def putShort(value: Short) = stream.writeShort(value)

  def putInt(value: Int) = stream.writeInt(value)

  def putLong(value: Long) = stream.writeLong(value)

  def putFloat(value: Float) = stream.writeFloat(value)

  def putDouble(value: Double) = stream.writeDouble(value)

  def putString(value: String) = stream.writeUTF(value)

}
