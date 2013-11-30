package scala.pickling

package binary {
  import collection.mutable.Buffer
  import UnsafeMemory._

  object Util {

    def copy(target: Array[Byte], i: Int, arr: Array[Byte]): Unit = {
      // def copy(src: Object, srcPos: Int, dest: Object, destPos: Int, length: Int)
      Array.copy(arr, 0, target, i, arr.length)
    }

    /** Returns decoded Short plus next "readable" position in target array.
     */
    def decodeShortFrom(arr: Array[Byte], i: Int): Short = {
      val fst = ((arr(i) << 8) & 0xFFFF).toShort
      val snd = (arr(i+1)      & 0x00FF).toShort
      (fst | snd).toShort
    }

    /** Returns decoded Char plus next "readable" position in target array.
     */
    def decodeCharFrom(arr: Array[Byte], i: Int): Char = {
      val fst = ((arr(i) << 8) & 0xFFFF).toChar
      val snd = (arr(i+1)      & 0x00FF).toChar
      (fst | snd).toChar
    }

    /** Returns decoded Int plus next "readable" position in target array.
     */
    def decodeIntFrom(arr: Array[Byte], i: Int): Int = {
      val fst = (arr(i) << 24).toInt
      val snd = ((arr(i+1) << 16) & 0x00FFFFFF).toInt
      val thrd = ((arr(i+2) << 8) & 0x0000FFFF).toInt
      val frth = (arr(i+3) & 0x000000FF).toInt
      fst | snd | thrd | frth
    }

    /** Returns decoded Long plus next "readable" position in target array.
     */
    def decodeLongFrom(arr: Array[Byte], i: Int): Long = {
      val elem1 = ((arr(i).toLong   << 56) & 0xFFFFFFFFFFFFFFFFL).toLong
      val elem2 = ((arr(i+1).toLong << 48) & 0x00FFFFFFFFFFFFFFL).toLong
      val elem3 = ((arr(i+2).toLong << 40) & 0x0000FFFFFFFFFFFFL).toLong
      val elem4 = ((arr(i+3).toLong << 32) & 0x000000FFFFFFFFFFL).toLong
      val elem5 = ((arr(i+4).toLong << 24) & 0x00000000FFFFFFFFL).toLong
      val elem6 = ((arr(i+5).toLong << 16) & 0x0000000000FFFFFFL).toLong
      val elem7 = ((arr(i+6).toLong << 8)  & 0x000000000000FFFFL).toLong
      val elem8 = (arr(i+7).toLong         & 0x00000000000000FFL).toLong
      elem1 | elem2 | elem3 | elem4 | elem5 | elem6 | elem7 | elem8
    }

    def fastdecodeIntFrom(arr: Array[Byte], i: Int): Int = {
      val fst = (getInt(arr, i) << 24).toInt
      val snd = ((getInt(arr, i+1) << 16) & 0x00FFFFFF).toInt
      val thrd = ((getInt(arr, i+2) << 8) & 0x0000FFFF).toInt
      val frth = (getInt(arr, i+3) & 0x000000FF).toInt
      fst | snd | thrd | frth
    }

    /** Returns next "writeable" position in target array.
     */
    def encodeShortTo(arr: Array[Byte], i: Int, value: Short): Int = {
      val fst = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val snd = (value & 0xff).asInstanceOf[Byte]
      arr(i) = fst
      arr(i+1) = snd
      i+2
    }

    /** Returns next "writeable" position in target array.
     */
    def encodeCharTo(arr: Array[Byte], i: Int, value: Char): Int = {
      val fst = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val snd = (value & 0xff).asInstanceOf[Byte]
      arr(i) = fst
      arr(i+1) = snd
      i+2
    }

    /** Returns next "writeable" position in target array.
     */
    def encodeIntTo(arr: Array[Byte], i: Int, value: Int): Int = {
      val fst = (value >>> 24).asInstanceOf[Byte]
      val snd = (value >>> 16 & 0xff).asInstanceOf[Byte]
      val thrd = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val frth = (value & 0xff).asInstanceOf[Byte]
      arr(i) = fst
      arr(i+1) = snd
      arr(i+2) = thrd
      arr(i+3) = frth
      i+4
    }

    /** Returns next "writeable" position in target array.
     */
    def encodeLongTo(arr: Array[Byte], i: Int, value: Long): Int = {
      val elem1 = (value >>> 56 & 0xff).asInstanceOf[Byte]
      val elem2 = (value >>> 48 & 0xff).asInstanceOf[Byte]
      val elem3 = (value >>> 40 & 0xff).asInstanceOf[Byte]
      val elem4 = (value >>> 32 & 0xff).asInstanceOf[Byte]
      val elem5 = (value >>> 24 & 0xff).asInstanceOf[Byte]
      val elem6 = (value >>> 16 & 0xff).asInstanceOf[Byte]
      val elem7 = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val elem8 = (value & 0xff).asInstanceOf[Byte]
      arr(i) = elem1
      arr(i+1) = elem2
      arr(i+2) = elem3
      arr(i+3) = elem4
      arr(i+4) = elem5
      arr(i+5) = elem6
      arr(i+6) = elem7
      arr(i+7) = elem8
      i+8
    }

    def fastencodeIntTo(arr: Array[Byte], i: Int, value: Int): Int = {
      val fst = (value >>> 24).asInstanceOf[Byte]
      val snd = (value >>> 16 & 0xff).asInstanceOf[Byte]
      val thrd = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val frth = (value & 0xff).asInstanceOf[Byte]
      putInt(arr, i, fst)
      putInt(arr, i+1, snd)
      putInt(arr, i+2, thrd)
      putInt(arr, i+3, frth)
      i+4
    }

    def encodeIntTo(buf: Buffer[Byte], i: Int, value: Int): Unit = {
      val fst = (value >>> 24).asInstanceOf[Byte]
      val snd = (value >>> 16 & 0xff).asInstanceOf[Byte]
      val thrd = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val frth = (value & 0xff).asInstanceOf[Byte]
      buf(i) = fst
      buf(i+1) = snd
      buf(i+2) = thrd
      buf(i+3) = frth
    }

    def encodeIntTo(buf: Buffer[Byte], value: Int): Unit = {
      val fst = (value >>> 24).asInstanceOf[Byte]
      val snd = (value >>> 16 & 0xff).asInstanceOf[Byte]
      val thrd = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val frth = (value & 0xff).asInstanceOf[Byte]
      buf += fst
      buf += snd
      buf += thrd
      buf += frth
    }

    def decodeBooleanFrom(arr: Array[Byte], i: Int): Boolean = {
      arr(i) != 0
    }

    def fastdecodeBooleanFrom(arr: Array[Byte], i: Int): Boolean = {
      getInt(arr, i) != 0
    }

    def encodeBooleanTo(arr: Array[Byte], i: Int, value: Boolean): Int = {
      arr(i) = if (value) 1 else 0
      i + 1
    }

    def fastencodeBooleanTo(arr: Array[Byte], i: Int, value: Boolean): Int = {
      putInt(arr, i, if (value) 1 else 0)
      i + 1
    }

    def encodeBooleanTo(buf: Buffer[Byte], i: Int, value: Boolean): Int = {
      buf += (if (value) 1 else 0)
      i + 1
    }

    def readBytesFrom(arr: Array[Byte], i: Int, len: Int): Array[Byte] = {
      val subarr = Array.ofDim[Byte](len)
      Array.copy(arr, i, subarr, 0, len)
      subarr
    }

    def decodeStringFrom(arr: Array[Byte], i: Int): (String, Int) = {
      val len = decodeIntFrom(arr, i)
      val bytes = Array.ofDim[Byte](len)
      Array.copy(arr, i + 4, bytes, 0, len)
      (new String(bytes, "UTF-8"), i + 4 + len)
    }

    def fastdecodeStringFrom(arr: Array[Byte], i: Int): (String, Int) = {
      val len = fastdecodeIntFrom(arr, i)
      val bytes = Array.ofDim[Byte](len)
      Array.copy(arr, i + 4, bytes, 0, len)
      (new String(bytes, "UTF-8"), i + 4 + len)
    }

    def encodeStringTo(arr: Array[Byte], i: Int, value: String): Int = {
      val bytes = value.getBytes("UTF-8")
      // encode length
      encodeIntTo(arr, i, bytes.length)
      // encode bytes of string at `next` position
      Util.copy(arr, i+4, bytes)
      i + 4 + bytes.length
    }

    def fastencodeStringTo(arr: Array[Byte], i: Int, value: String): Int = {
      val bytes = value.getBytes("UTF-8")
      // encode length
      val next = fastencodeIntTo(arr, i, bytes.length)
      // encode bytes of string at `next` position
      Util.copy(arr, next, bytes)
      next + bytes.length
    }

    def encodeStringTo(buf: Buffer[Byte], i: Int, value: String): Int = {
      val bytes = value.getBytes("UTF-8")
      // encode length
      encodeIntTo(buf, bytes.length) // requires 4 bytes
      // append bytes of string to `buf`
      buf ++= bytes
      i + 4 + bytes.length
    }
    
    def encodeByteArrayTo(arr: Array[Byte], i: Int, value: Array[Byte]): Int = {
      encodeIntTo(arr, i, value.length)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i + 4, value.length * 1)
      i + 4 + value.length * 1
    }
    
    def encodeShortArrayTo(arr: Array[Byte], i: Int, value: Array[Short]): Int = {
      encodeIntTo(arr, i, value.length)
      val srcOffset = UnsafeMemory.shortArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i + 4, value.length * 2)
      i + 4 + value.length * 2
    }
    
    def encodeCharArrayTo(arr: Array[Byte], i: Int, value: Array[Char]): Int = {
      encodeIntTo(arr, i, value.length)
      val srcOffset = UnsafeMemory.charArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i + 4, value.length * 2)
      i + 4 + value.length * 2
    }
    
    def encodeIntArrayTo(arr: Array[Byte], i: Int, value: Array[Int]): Int = {
      encodeIntTo(arr, i, value.length)
      val srcOffset = UnsafeMemory.intArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i + 4, value.length * 4)
      i + 4 + value.length * 4
    }
    
    def encodeLongArrayTo(arr: Array[Byte], i: Int, value: Array[Long]): Int = {
      encodeIntTo(arr, i, value.length)
      val srcOffset = UnsafeMemory.longArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i + 4, value.length * 8)
      i + 4 + value.length * 8
    }
    
    def encodeBooleanArrayTo(arr: Array[Byte], i: Int, value: Array[Boolean]): Int = {
      encodeIntTo(arr, i, value.length)
      val srcOffset = UnsafeMemory.booleanArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i + 4, value.length * 1)
      i + 4 + value.length * 1
    }
    
    def encodeFloatArrayTo(arr: Array[Byte], i: Int, value: Array[Float]): Int = {
      encodeIntTo(arr, i, value.length)
      val srcOffset = UnsafeMemory.floatArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i + 4, value.length * 4)
      i + 4 + value.length * 4
    }
    
    def encodeDoubleArrayTo(arr: Array[Byte], i: Int, value: Array[Double]): Int = {
      encodeIntTo(arr, i, value.length)
      val srcOffset = UnsafeMemory.doubleArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i + 4, value.length * 8)
      i + 4 + value.length * 8
    }
    
    def decodeByteArrayFrom(arr: Array[Byte], i: Int): (Array[Byte], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i+4
      val ia = Array.ofDim[Byte](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 1)
      (ia, nextPos + len * 1)
    }
    
    def decodeShortArrayFrom(arr: Array[Byte], i: Int): (Array[Short], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i+4
      val ia = Array.ofDim[Short](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.shortArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 2)
      (ia, nextPos + len * 2)
    }

    def decodeCharArrayFrom(arr: Array[Byte], i: Int): (Array[Char], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i+4
      val ia = Array.ofDim[Char](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.charArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 2)
      (ia, nextPos + len * 2)
    }

    def decodeIntArrayFrom(arr: Array[Byte], i: Int): (Array[Int], Int) = {

      val len = decodeIntFrom(arr, i)
      val nextPos = i+4
      val ia = Array.ofDim[Int](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.intArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 4)

      (ia, nextPos + len * 4)
    }

    def decodeLongArrayFrom(arr: Array[Byte], i: Int): (Array[Long], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i+4
      val ia = Array.ofDim[Long](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.longArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 8)
      (ia, nextPos + len * 8)
    }

    def decodeBooleanArrayFrom(arr: Array[Byte], i: Int): (Array[Boolean], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i+4
      val ia = Array.ofDim[Boolean](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.booleanArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 1)
      (ia, nextPos + len * 1)
    }

    def decodeFloatArrayFrom(arr: Array[Byte], i: Int): (Array[Float], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i+4
      val ia = Array.ofDim[Float](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.floatArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 4)
      (ia, nextPos + len * 4)
    }

    def decodeDoubleArrayFrom(arr: Array[Byte], i: Int): (Array[Double], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i+4
      val ia = Array.ofDim[Double](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.doubleArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + nextPos, ia, destOffset, len * 8)
      (ia, nextPos + len * 8)
    }
  }

  object UnsafeMemory {
    import sun.misc.Unsafe

    private[binary] val unsafe: Unsafe =
      scala.concurrent.util.Unsafe.instance

    private[binary] val byteArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Byte]])
    private[binary] val shortArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Short]])
    private[binary] val charArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Char]])
    private[binary] val intArrayOffset: Long  = unsafe.arrayBaseOffset(classOf[Array[Int]])
    private[binary] val longArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Long]])
    private[binary] val booleanArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Boolean]])
    private[binary] val floatArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Float]])
    private[binary] val doubleArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Double]])

    def putInt(buffer: Array[Byte], i: Int, value: Int): Unit = {
      unsafe.putInt(buffer, byteArrayOffset + i, value)
    }

    def getInt(buffer: Array[Byte], i: Int): Int = {
      unsafe.getInt(buffer, byteArrayOffset + i)
    }
  }

}
