package scala.pickling.binary

abstract class BinaryOutput {

  def result: Option[Array[Byte]]

  def ensureCapacity(capacity: Int): Unit

  def putBoolean(value: Boolean): Unit

  def putByte(value: Byte): Unit

  def putChar(value: Char): Unit

  def putShort(value: Short): Unit

  def putInt(value: Int): Unit

  def putLong(value: Long): Unit

  def putFloat(value: Float): Unit

  def putDouble(value: Double): Unit

  def putString(value: String) {
    val bytes = value.getBytes("UTF-8")
    putByteArray(bytes)
  }
  

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

class ByteBufferOutput(_buffer: java.nio.ByteBuffer) extends BinaryOutput {

  import java.nio.ByteOrder
  import java.nio.ByteBuffer
  
  def result = None

  private var buffer = _buffer
  assert(buffer.order == ByteOrder.BIG_ENDIAN)

  private def growTo(newSize: Int) {
    assert(newSize > 0)
    val newBuffer =
      if (buffer.isDirect) ByteBuffer.allocateDirect(newSize)
      else ByteBuffer.allocate(newSize)
    //copy the content
    val pos = buffer.position
    buffer.limit(pos)
    buffer.position(0)
    newBuffer.put(buffer)
    buffer = newBuffer
  }


  private def grow {
    val newSize = 2*buffer.capacity
    growTo(newSize)
  }

  def ensureCapacity(capacity: Int) {
    val left = buffer.remaining
    if (left < capacity) {
      growTo(buffer.capacity + (capacity - left))
    }
  }

  @inline private def withReallocate[A](op: A => ByteBuffer, value: A) {
    while(true) {
      buffer.mark
      try {
        op(value)
        return
      } catch {
        case _: java.nio.BufferOverflowException =>
          buffer.reset
          grow
      }
    }
  }

  def putBoolean(value: Boolean) = withReallocate[Byte](buffer.put, value.asInstanceOf[Byte])

  def putByte(value: Byte) =  withReallocate[Byte](buffer.put, value)

  def putChar(value: Char) = withReallocate(buffer.putChar, value)

  def putShort(value: Short) = withReallocate(buffer.putShort, value)

  def putInt(value: Int) = withReallocate(buffer.putInt, value)

  def putLong(value: Long) = withReallocate(buffer.putLong, value)

  def putFloat(value: Float) = withReallocate(buffer.putFloat, value)

  def putDouble(value: Double) = withReallocate(buffer.putDouble, value)

  override def putByteArray(value: Array[Byte]): Unit = {
    def process(value: Array[Byte]) = {
      putInt(value.size)
      buffer.put(value)
    }
    withReallocate(process, value)
  }

}

class ByteArrayOutput(initialCapacity: Int = 1024) extends BinaryOutput {

  val buffer = new java.io.ByteArrayOutputStream(initialCapacity)

  def result = Some(buffer.toByteArray)
  
  def ensureCapacity(capacity: Int) {
    //TODO ignore for the moment, not sure if it is worth creating a new buffer and copying the old one
  }

  def putBoolean(value: Boolean) {
    if (value) buffer.write(1)
    else buffer.write(0)
  }

  def putByte(value: Byte) {
    buffer.write(value)
  }

  def putChar(value: Char) {
    buffer.write(value >>> 8 & 0xff)
    buffer.write(value & 0xff)
  }

  def putShort(value: Short) {
    buffer.write(value >>> 8 & 0xff)
    buffer.write(value & 0xff)
  }

  def putInt(value: Int) {
    buffer.write(value >>> 24)
    buffer.write(value >>> 16 & 0xff)
    buffer.write(value >>> 8 & 0xff)
    buffer.write(value & 0xff)
  }

  def putLong(value: Long) {
    buffer.write((value >>> 56 & 0xff).asInstanceOf[Int])
    buffer.write((value >>> 48 & 0xff).asInstanceOf[Int])
    buffer.write((value >>> 40 & 0xff).asInstanceOf[Int])
    buffer.write((value >>> 32 & 0xff).asInstanceOf[Int])
    buffer.write((value >>> 24 & 0xff).asInstanceOf[Int])
    buffer.write((value >>> 16 & 0xff).asInstanceOf[Int])
    buffer.write((value >>> 8 & 0xff).asInstanceOf[Int])
    buffer.write((value & 0xff).asInstanceOf[Int])
  }

  def putFloat(value: Float) {
    val intValue = java.lang.Float.floatToRawIntBits(value)
    putInt(intValue)
  }

  def putDouble(value: Double) {
    val longValue = java.lang.Double.doubleToRawLongBits(value)
    putLong(longValue)
  }

  override def putByteArray(value: Array[Byte]): Unit = {
    putInt(value.length)
    buffer.write(value)
  }
  
  //TODO override array

}

class PickleArrayOutput(buffer: scala.pickling.ArrayOutput[Byte]) extends BinaryOutput {
  
  def result = {
    val res = buffer.result
    if (res == null) None
    else Some(res)
  }
  
  def ensureCapacity(capacity: Int) {
    //TODO ignore for the moment, not sure if it is worth creating a new buffer and copying the old one
  }

  @inline private def write(i: Int) {
    buffer += i.asInstanceOf[Byte]
  }

  def putBoolean(value: Boolean) {
    if (value) write(1)
    else write(0)
  }

  def putByte(value: Byte) {
    write(value)
  }

  def putChar(value: Char) {
    write(value >>> 8 & 0xff)
    write(value & 0xff)
  }

  def putShort(value: Short) {
    write(value >>> 8 & 0xff)
    write(value & 0xff)
  }

  def putInt(value: Int) {
    write(value >>> 24)
    write(value >>> 16 & 0xff)
    write(value >>> 8 & 0xff)
    write(value & 0xff)
  }

  def putLong(value: Long) {
    write((value >>> 56 & 0xff).asInstanceOf[Int])
    write((value >>> 48 & 0xff).asInstanceOf[Int])
    write((value >>> 40 & 0xff).asInstanceOf[Int])
    write((value >>> 32 & 0xff).asInstanceOf[Int])
    write((value >>> 24 & 0xff).asInstanceOf[Int])
    write((value >>> 16 & 0xff).asInstanceOf[Int])
    write((value >>> 8 & 0xff).asInstanceOf[Int])
    write((value & 0xff).asInstanceOf[Int])
  }

  def putFloat(value: Float) {
    val intValue = java.lang.Float.floatToRawIntBits(value)
    putInt(intValue)
  }

  def putDouble(value: Double) {
    val longValue = java.lang.Double.doubleToRawLongBits(value)
    putLong(longValue)
  }

  override def putByteArray(value: Array[Byte]): Unit = {
    putInt(value.length)
    buffer.put(value)
  }

}

//  class DataStreamOutput(stream: java.io.DataOutputStream) extends BinaryOutput {
//    
//    def putBoolean(value: Boolean) = stream.writeBoolean(value)

//    def putByte(value: Byte) = stream.writeByte(value)

//    def putChar(value: Char) = stream.writeChar(value)

//    def putShort(value: Short) = stream.writeShort(value)

//    def putInt(value: Int) = stream.writeInt(value)

//    def putLong(value: Long) = stream.writeLong(value)

//    def putFloat(value: Float) = stream.writeFloat(value)

//    def putDouble(value: Double) = stream.writeDouble(value)

//    override def putString(value: String) = stream.writeUTF(value)

//  }
