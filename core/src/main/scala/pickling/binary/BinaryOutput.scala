package scala.pickling.binary

abstract class BinaryOutput {

  def result: Option[Array[Byte]]

  def ensureCapacity(capacity: Int): Unit

  def putBoolean(value: Boolean): Unit = {
    if (value) putByte(1)
    else putByte(0)
  }

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

  var buffer = _buffer
  assert(buffer.order == ByteOrder.BIG_ENDIAN)

  private def growTo(newSize: Int) {
    //println("growing to " + newSize)
    assert(newSize > 0) //can we overflow before running out of memory ?
    val newBuffer =
      if (buffer.isDirect) ByteBuffer.allocateDirect(newSize)
      else ByteBuffer.allocate(newSize)
    //copy the content
    val pos = buffer.position
    buffer.limit(pos)
    buffer.position(0)
    newBuffer.put(buffer)
    buffer = newBuffer
    //assert(newBuffer.position == pos)
    //assert((0 until pos).forall(i => buffer.get(i) == newBuffer.get(i)))
    //println("capapcity = " + buffer.capacity)
    //println(buffer.toString)
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
        assert(false, "after return ?")
      } catch {
        case _: java.nio.BufferOverflowException =>
          buffer.reset
          grow
      }
    }
  }


  @inline private def bb(i: Byte) = { buffer.put(i) }
  def putByte(value: Byte) =  withReallocate[Byte](bb, value)

  @inline private def cc(i: Char) = { buffer.putChar(i) }
  def putChar(value: Char) = withReallocate(cc, value)

  @inline private def ss(i: Short) = { buffer.putShort(i) }
  def putShort(value: Short) = withReallocate(ss, value)

  @inline private def ii(i: Int) = { buffer.putInt(i) }
  def putInt(value: Int) = withReallocate(ii, value)

  @inline private def ll(i: Long) = { buffer.putLong(i) }
  def putLong(value: Long) = withReallocate(ll, value)

  @inline private def ff(i: Float) = { buffer.putFloat(i) }
  def putFloat(value: Float) = withReallocate(ff, value)

  @inline private def dd(i: Double) = { buffer.putDouble(i) }
  def putDouble(value: Double) = withReallocate(dd, value)

  @inline private def pba(value: Array[Byte]) = { buffer.put(value) }
  override def putByteArray(value: Array[Byte]): Unit = {
    putInt(value.size)
    withReallocate(pba, value)
  }

}

class ByteArrayOutput(initialCapacity: Int = 1024) extends BinaryOutput {

  val buffer = new java.io.ByteArrayOutputStream(initialCapacity)

  def result = Some(buffer.toByteArray)
  
  def ensureCapacity(capacity: Int) {
    //TODO ignore for the moment, not sure if it is worth creating a new buffer and copying the old one
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

class StreamOutput(stream: java.io.OutputStream) extends BinaryOutput {
  val ds = new java.io.DataOutputStream(stream)
  def result = None
  def ensureCapacity(capacity: Int) { }
  def putByte(value: Byte) = ds.writeByte(value)
  def putChar(value: Char) = ds.writeChar(value)
  def putShort(value: Short) = ds.writeShort(value)
  def putInt(value: Int) = ds.writeInt(value)
  def putLong(value: Long) = ds.writeLong(value)
  def putFloat(value: Float) = ds.writeFloat(value)
  def putDouble(value: Double) = ds.writeDouble(value)
  
  override def putByteArray(value: Array[Byte]): Unit = {
    putInt(value.length)
    ds.write(value)
  }

}
