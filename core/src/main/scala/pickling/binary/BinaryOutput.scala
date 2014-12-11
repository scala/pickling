package scala.pickling.binary

abstract class BinaryOutput {

  def result: Array[Byte] //can be null

  def ensureCapacity(capacity: Int): Unit

  def putByte(value: Byte): Unit

  def putChar(value: Char): Unit

  def putShort(value: Short): Unit

  def putInt(value: Int): Unit

  def putLong(value: Long): Unit

  def putFloat(value: Float): Unit

  def putDouble(value: Double): Unit

  def putBytes(bytes: Array[Byte], len: Int): Unit

  ////////////////////////
  // Derived operations //
  ////////////////////////
  
  def putBoolean(value: Boolean): Unit = {
    if (value) putByte(1)
    else putByte(0)
  }

  def putString(value: String) {
    val bytes = value.getBytes("UTF-8")
    putByteArray(bytes)
  }

  protected val chunkSize = 1024
  protected val chunk = Array.ofDim[Byte](chunkSize)

  protected def putArrayByChunk[T <: AnyVal](arr: Array[T], offset: Long, eltSize: Int) {
    val nbrElt = arr.length
    putInt(nbrElt)
    var srcOffset = offset //UnsafeMemory.byteArrayOffset
    var toCopy = nbrElt * eltSize
    while (toCopy > 0) {
      val byteLen = math.min(chunkSize, toCopy)
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset, chunk, UnsafeMemory.byteArrayOffset, byteLen)
      toCopy -= byteLen
      srcOffset += byteLen
      putBytes(chunk, byteLen)
    }
  }

  def putByteArray(value: Array[Byte]): Unit = {
    val size = value.size
    putInt(size)
    putBytes(value, size)
  }
  
  def putBooleanArray(value: Array[Boolean]): Unit = putArrayByChunk(value, UnsafeMemory.booleanArrayOffset, 1)
  def putCharArray(value: Array[Char]): Unit = putArrayByChunk(value, UnsafeMemory.charArrayOffset, 2)
  def putShortArray(value: Array[Short]): Unit = putArrayByChunk(value, UnsafeMemory.shortArrayOffset, 2)
  def putIntArray(value: Array[Int]): Unit = putArrayByChunk(value, UnsafeMemory.intArrayOffset, 4)
  def putFloatArray(value: Array[Float]): Unit = putArrayByChunk(value, UnsafeMemory.floatArrayOffset, 4)
  def putLongArray(value: Array[Long]): Unit = putArrayByChunk(value, UnsafeMemory.longArrayOffset, 8)
  def putDoubleArray(value: Array[Double]): Unit = putArrayByChunk(value, UnsafeMemory.doubleArrayOffset, 8)

}

class ByteBufferOutput(buffer: java.nio.ByteBuffer) extends BinaryOutput {

  import java.nio.ByteOrder
  import java.nio.ByteBuffer

  assert(buffer.order == ByteOrder.BIG_ENDIAN)
  
  def result: Array[Byte] = null

  def ensureCapacity(capacity: Int) {
    if (buffer.remaining < capacity)
      throw new java.nio.BufferOverflowException()
  }

  def putByte(value: Byte) = buffer.put(value)
  def putChar(value: Char) = buffer.putChar(value)
  def putShort(value: Short) = buffer.putShort(value)
  def putInt(value: Int) = buffer.putInt(value)
  def putLong(value: Long) = buffer.putLong(value)
  def putFloat(value: Float) = buffer.putFloat(value)
  def putDouble(value: Double) = buffer.putDouble(value)
  def putBytes(value: Array[Byte], len: Int) = buffer.put(value, 0, len)
}

class StreamOutput(stream: java.io.OutputStream) extends BinaryOutput {
  val ds = new java.io.DataOutputStream(stream)
  def result: Array[Byte] = null
  def ensureCapacity(capacity: Int) { }
  def putByte(value: Byte) = ds.writeByte(value)
  def putChar(value: Char) = ds.writeChar(value)
  def putShort(value: Short) = ds.writeShort(value)
  def putInt(value: Int) = ds.writeInt(value)
  def putLong(value: Long) = ds.writeLong(value)
  def putFloat(value: Float) = ds.writeFloat(value)
  def putDouble(value: Double) = ds.writeDouble(value)
  def putBytes(value: Array[Byte], len: Int) = ds.write(value, 0, len)
}

class ByteArrayOutput(buffer: java.io.ByteArrayOutputStream) extends StreamOutput(buffer) {
  def this(initialCapacity: Int) = this(new java.io.ByteArrayOutputStream(initialCapacity))
  def this() = this(1024)
  override def result = buffer.toByteArray
}

class FixedByteArrayOutput(capacity: Int) extends BinaryOutput {

  private val head = Array.ofDim[Byte](capacity)
  private var pos = 0

  override def result = head

  def ensureCapacity(capacity: Int): Unit = { }

  def putByte(value: Byte) {
    head(pos) = value
    pos += 1
  }

  def putChar(value: Char) {
    head(pos)   = (value >>> 8 & 0xff).asInstanceOf[Byte]
    head(pos+1) = (value & 0xff).asInstanceOf[Byte]
    pos += 2
  }

  def putShort(value: Short) {
    head(pos)   = (value >>> 8 & 0xff).asInstanceOf[Byte]
    head(pos+1) = (value & 0xff).asInstanceOf[Byte]
    pos += 2
  }

  def putInt(value: Int) {
    head(pos)   = (value >>> 24 & 0xff).asInstanceOf[Byte]
    head(pos+1) = (value >>> 16 & 0xff).asInstanceOf[Byte]
    head(pos+2) = (value >>>  8 & 0xff).asInstanceOf[Byte]
    head(pos+3) = (value        & 0xff).asInstanceOf[Byte]
    pos += 4
  }

  def putLong(value: Long) {
    head(pos)   = (value >>> 56 & 0xff).asInstanceOf[Byte]
    head(pos+1) = (value >>> 48 & 0xff).asInstanceOf[Byte]
    head(pos+2) = (value >>> 40 & 0xff).asInstanceOf[Byte]
    head(pos+3) = (value >>> 32 & 0xff).asInstanceOf[Byte]
    head(pos+4) = (value >>> 24 & 0xff).asInstanceOf[Byte]
    head(pos+5) = (value >>> 16 & 0xff).asInstanceOf[Byte]
    head(pos+6) = (value >>>  8 & 0xff).asInstanceOf[Byte]
    head(pos+7) = (value        & 0xff).asInstanceOf[Byte]
    pos += 8
  }

  def putFloat(value: Float) {
    val intValue = java.lang.Float.floatToRawIntBits(value)
    putInt(intValue)
  }

  def putDouble(value: Double) {
    val longValue = java.lang.Double.doubleToRawLongBits(value)
    putLong(longValue)
  }

  def putBytes(value: Array[Byte], len: Int): Unit = {
    val off = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(value, off, head, off + pos, len)
    pos += len
  }

  //a single chunk
  override protected def putArrayByChunk[T <: AnyVal](arr: Array[T], offset: Long, eltSize: Int) {
    val nbrElt = arr.length
    var byteLen = nbrElt * eltSize
    putInt(nbrElt)
    UnsafeMemory.unsafe.copyMemory(arr, offset, head, UnsafeMemory.byteArrayOffset + pos, byteLen)
    pos += byteLen
  }

}

//TODO as a pool rather than a single array
//TODO that might be dangerous in terms of security (exfiltrate data through the preAlloc array)
object FastByteArrayOutput {

  private val lock = new java.util.concurrent.locks.ReentrantLock()
  private var preAlloc = Array.ofDim[Byte](64 * 1024 * 1024) // 64 MB
  
  def get = {
    lock.lock
    try {
      val p = preAlloc
      preAlloc = null
      p
    } finally {
      lock.unlock
    }
  }
  
  def set(p: Array[Byte]) = {
    lock.lock
    try {
      preAlloc = p
    } finally {
      lock.unlock
    }
  }

}

class FastByteArrayOutput(initialCapacity: Int = 10 * 1024 * 1024) extends BinaryOutput {

  override protected val chunkSize = 1024 * 1024
  override protected val chunk = null
  private val allowedWaste = 512

  private var pos = 0 //current position in the head
  private var head: Array[Byte] = null
  private var preA: Array[Byte] = null
  private var chunks = List[(Int, Array[Byte])]()
  private var stored = 0 //current size

  def init() {
    val h = FastByteArrayOutput.get
    if (h != null && h.size >= initialCapacity) {
      head = h
      preA = h
    } else {
      FastByteArrayOutput.set(h)
      head = new Array[Byte](initialCapacity)
    }
  }

  init()

  def result = {
    val size = pos + stored
    val toCopy = ((pos -> head) :: chunks).reverse
    var idx = 0
    val target = Array.ofDim[Byte](size)
    val off = UnsafeMemory.byteArrayOffset
    for ( (size, arr) <- toCopy ) {
      UnsafeMemory.unsafe.copyMemory(arr, off, target, off + idx, size)
      idx += size
    }
    //release resources
    if (preA != null) {
      FastByteArrayOutput.set(preA)
    }
    head = null
    chunks = Nil
    //
    target
  }
  
  def ensureCapacity(capacity: Int) {
    val avail = head.size - pos
    val need = capacity - avail
    if (need > 0) {
      if (pos == 0) {
        head = Array.ofDim[Byte](capacity)
      } else if (avail > allowedWaste && head.size != chunkSize) {
        val newHead = Array.ofDim[Byte](math.max(chunkSize, need))
        val off = UnsafeMemory.byteArrayOffset
        UnsafeMemory.unsafe.copyMemory(head, off, newHead, off, pos)
        head = newHead
      } else {
        chunks = (pos -> head) :: chunks
        head = Array.ofDim[Byte](math.max(chunkSize, need))
        stored += pos
        pos = 0
      }
    }
  }

  def putByte(value: Byte) {
    ensureCapacity(1)
    head(pos) = value
    pos += 1
  }

  def putChar(value: Char) {
    ensureCapacity(2)
    head(pos)   = (value >>> 8 & 0xff).asInstanceOf[Byte]
    head(pos+1) = (value & 0xff).asInstanceOf[Byte]
    pos += 2
  }

  def putShort(value: Short) {
    ensureCapacity(2)
    head(pos)   = (value >>> 8 & 0xff).asInstanceOf[Byte]
    head(pos+1) = (value & 0xff).asInstanceOf[Byte]
    pos += 2
  }

  def putInt(value: Int) {
    ensureCapacity(4)
    head(pos)   = (value >>> 24 & 0xff).asInstanceOf[Byte]
    head(pos+1) = (value >>> 16 & 0xff).asInstanceOf[Byte]
    head(pos+2) = (value >>>  8 & 0xff).asInstanceOf[Byte]
    head(pos+3) = (value        & 0xff).asInstanceOf[Byte]
    pos += 4
  }

  def putLong(value: Long) {
    ensureCapacity(8)
    head(pos)   = (value >>> 56 & 0xff).asInstanceOf[Byte]
    head(pos+1) = (value >>> 48 & 0xff).asInstanceOf[Byte]
    head(pos+2) = (value >>> 40 & 0xff).asInstanceOf[Byte]
    head(pos+3) = (value >>> 32 & 0xff).asInstanceOf[Byte]
    head(pos+4) = (value >>> 24 & 0xff).asInstanceOf[Byte]
    head(pos+5) = (value >>> 16 & 0xff).asInstanceOf[Byte]
    head(pos+6) = (value >>>  8 & 0xff).asInstanceOf[Byte]
    head(pos+7) = (value        & 0xff).asInstanceOf[Byte]
    pos += 8
  }

  def putFloat(value: Float) {
    val intValue = java.lang.Float.floatToRawIntBits(value)
    putInt(intValue)
  }

  def putDouble(value: Double) {
    val longValue = java.lang.Double.doubleToRawLongBits(value)
    putLong(longValue)
  }
  
  def putBytes(value: Array[Byte], len: Int): Unit = {
    ensureCapacity(len)
    val off = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(value, off, head, off + pos, len)
    pos += len
  }

  //a single chunk
  override protected def putArrayByChunk[T <: AnyVal](arr: Array[T], offset: Long, eltSize: Int) {
    val nbrElt = arr.length
    var byteLen = nbrElt * eltSize
    ensureCapacity(byteLen+4)
    putInt(nbrElt)
    UnsafeMemory.unsafe.copyMemory(arr, offset, head, UnsafeMemory.byteArrayOffset + pos, byteLen)
    pos += byteLen
  }

}

