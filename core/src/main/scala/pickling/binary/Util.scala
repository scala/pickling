package scala.pickling

package binary {
  import collection.mutable.Buffer
  import UnsafeMemory._

  object Util {

    val SizeOfByte = 1
    val SizeOfShort = 2
    val SizeOfInt = 4
    val SizeOfLong = 8
    val SizeOfFloat  = 4
    val SizeOfDouble = 8
    val SizeOfChar = 2
    val SizeOfBoolean = 1

    // Decoding functions

    def decodeShortFrom(arr: Array[Byte], i: Int): Short = {
      val fst = ((arr(i) << 8) & 0xFFFF).toShort
      val snd = (arr(i+1)      & 0x00FF).toShort
      (fst | snd).toShort
    }

    def decodeIntFrom(arr: Array[Byte], i: Int): Int = {
      val fst = (arr(i) << 24).toInt
      val snd = ((arr(i+1) << 16) & 0x00FFFFFF).toInt
      val thrd = ((arr(i+2) << 8) & 0x0000FFFF).toInt
      val frth = (arr(i+3) & 0x000000FF).toInt
      fst | snd | thrd | frth
    }

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

    def decodeCharFrom(arr: Array[Byte], i: Int): Char = {
      val fst = ((arr(i) << 8) & 0xFFFF).toChar
      val snd = (arr(i+1)      & 0x00FF).toChar
      (fst | snd).toChar
    }

    def decodeStringFrom(arr: Array[Byte], i: Int): (String, Int) = {
      val len = decodeIntFrom(arr, i)
      val bytes = Array.ofDim[Byte](len)
      Array.copy(arr, i + SizeOfInt, bytes, 0, len)
      (new String(bytes, "UTF-8"), i + SizeOfInt + len)
    }

    def decodeBooleanFrom(arr: Array[Byte], i: Int): Boolean = {
      arr(i) != 0
    }

    def decodeByteArray(arr: Array[Byte], offset: Int, len: Int): Array[Byte] = {
      val newArr = Array.ofDim[Byte](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + offset, newArr, destOffset, len * SizeOfByte)
      newArr
    }

    def decodeByteArrayFrom(arr: Array[Byte], i: Int): (Array[Byte], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i + SizeOfInt
      val ia = decodeByteArray(arr, nextPos, len)
      (ia, nextPos + len * SizeOfByte)
    }

    def decodeShortArray(arr: Array[Byte], offset: Int, len: Int): Array[Short] = {
      val newArr = Array.ofDim[Short](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.shortArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + offset, newArr, destOffset, len * SizeOfShort)
      newArr
    }

    def decodeShortArrayFrom(arr: Array[Byte], i: Int): (Array[Short], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i + SizeOfInt
      val ia = decodeShortArray(arr, nextPos, len)
      (ia, nextPos + len * SizeOfShort)
    }

    def decodeCharArray(arr: Array[Byte], offset: Int, len: Int): Array[Char] = {
      val newArr = Array.ofDim[Char](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.charArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + offset, newArr, destOffset, len * SizeOfChar)
      newArr
    }

    def decodeCharArrayFrom(arr: Array[Byte], i: Int): (Array[Char], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i + SizeOfInt
      val ia = decodeCharArray(arr, nextPos, len)
      (ia, nextPos + len * SizeOfChar)
    }

    def decodeIntArray(arr: Array[Byte], offset: Int, len: Int): Array[Int] = {
      val newArr = Array.ofDim[Int](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.intArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + offset, newArr, destOffset, len * SizeOfInt)
      newArr
    }

    def decodeIntArrayFrom(arr: Array[Byte], i: Int): (Array[Int], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i + SizeOfInt
      val ia = decodeIntArray(arr, nextPos, len)
      (ia, nextPos + len * SizeOfInt)
    }

    def decodeLongArray(arr: Array[Byte], offset: Int, len: Int): Array[Long] = {
      val newArr = Array.ofDim[Long](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.longArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + offset, newArr, destOffset, len * SizeOfLong)
      newArr
    }

    def decodeLongArrayFrom(arr: Array[Byte], i: Int): (Array[Long], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i + SizeOfInt
      val ia = decodeLongArray(arr, nextPos, len)
      (ia, nextPos + len * SizeOfLong)
    }

    def decodeFloatArray(arr: Array[Byte], offset: Int, len: Int): Array[Float] = {
      val newArr = Array.ofDim[Float](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.floatArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + offset, newArr, destOffset, len * SizeOfFloat)
      newArr
    }

    def decodeFloatArrayFrom(arr: Array[Byte], i: Int): (Array[Float], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i + SizeOfInt
      val ia = decodeFloatArray(arr, nextPos, len)
      (ia, nextPos + len * SizeOfFloat)
    }

    def decodeDoubleArray(arr: Array[Byte], offset: Int, len: Int): Array[Double] = {
      val newArr = Array.ofDim[Double](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.doubleArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + offset, newArr, destOffset, len * SizeOfDouble)
      newArr
    }

    def decodeDoubleArrayFrom(arr: Array[Byte], i: Int): (Array[Double], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i + SizeOfInt
      val ia = decodeDoubleArray(arr, nextPos, len)
      (ia, nextPos + len * SizeOfDouble)
    }

    def decodeBooleanArray(arr: Array[Byte], offset: Int, len: Int): Array[Boolean] = {
      val newArr = Array.ofDim[Boolean](len)
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.booleanArrayOffset
      UnsafeMemory.unsafe.copyMemory(arr, srcOffset + offset, newArr, destOffset, len * SizeOfBoolean)
      newArr
    }

    def decodeBooleanArrayFrom(arr: Array[Byte], i: Int): (Array[Boolean], Int) = {
      val len = decodeIntFrom(arr, i)
      val nextPos = i + SizeOfInt
      val ia = decodeBooleanArray(arr, nextPos, len)
      (ia, nextPos + len * SizeOfBoolean)
    }

    // Encoding functions

    def encodeByte(arr: ArrayOutput[Byte], value: Byte): Unit = {
      arr += value
    }

    def encodeShort(arr: ArrayOutput[Byte], value: Short): Unit = {
      val fst = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val snd = (value & 0xff).asInstanceOf[Byte]
      arr += fst
      arr += snd
    }

    def encodeInt(arr: ArrayOutput[Byte], value: Int): Unit = {
      val fst = (value >>> 24).asInstanceOf[Byte]
      val snd = (value >>> 16 & 0xff).asInstanceOf[Byte]
      val thrd = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val frth = (value & 0xff).asInstanceOf[Byte]
      arr += fst
      arr += snd
      arr += thrd
      arr += frth
    }


    def encodeLong(arr: ArrayOutput[Byte], value: Long): Unit = {
      val elem1 = (value >>> 56 & 0xff).asInstanceOf[Byte]
      val elem2 = (value >>> 48 & 0xff).asInstanceOf[Byte]
      val elem3 = (value >>> 40 & 0xff).asInstanceOf[Byte]
      val elem4 = (value >>> 32 & 0xff).asInstanceOf[Byte]
      val elem5 = (value >>> 24 & 0xff).asInstanceOf[Byte]
      val elem6 = (value >>> 16 & 0xff).asInstanceOf[Byte]
      val elem7 = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val elem8 = (value & 0xff).asInstanceOf[Byte]
      arr += elem1
      arr += elem2
      arr += elem3
      arr += elem4
      arr += elem5
      arr += elem6
      arr += elem7
      arr += elem8
    }

    def encodeChar(arr: ArrayOutput[Byte], value: Char): Unit = {
      val fst = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val snd = (value & 0xff).asInstanceOf[Byte]
      arr += fst
      arr += snd
    }

    def encodeString(arr: ArrayOutput[Byte], value: String): Unit = {
      val bytes = value.getBytes("UTF-8")
      encodeInt(arr, bytes.length)
      val (buf, pos) = arr.target(bytes.length * SizeOfByte)
      encodeByteArrayTo(buf, pos, bytes)
      arr.flush(buf)
    }

    def encodeBoolean(arr: ArrayOutput[Byte], value: Boolean): Unit = {
      arr += (if (value) 1 else 0)
    }

    def encodeByteArray(arr: ArrayOutput[Byte], value: Array[Byte]): Unit = {
      encodeInt(arr, value.length)
      val (buf, pos) = arr.target(value.length * SizeOfByte)
      encodeByteArrayTo(buf, pos, value)
      arr.flush(buf)
    }

    def encodeShortArray(arr: ArrayOutput[Byte], value: Array[Short]): Unit = {
      encodeInt(arr, value.length)
      val (buf, pos) = arr.target(value.length * SizeOfShort)
      encodeShortArrayTo(buf, pos, value)
      arr.flush(buf)
    }

    def encodeIntArray(arr: ArrayOutput[Byte], value: Array[Int]): Unit = {
      encodeInt(arr, value.length)
      val (buf, pos) = arr.target(value.length * SizeOfInt)
      encodeIntArrayTo(buf, pos, value)
      arr.flush(buf)
    }

    def encodeLongArray(arr: ArrayOutput[Byte], value: Array[Long]): Unit = {
      encodeInt(arr, value.length)
      val (buf, pos) = arr.target(value.length * SizeOfLong)
      encodeLongArrayTo(buf, pos, value)
      arr.flush(buf)
    }

    def encodeFloatArray(arr: ArrayOutput[Byte], value: Array[Float]): Unit = {
      encodeInt(arr, value.length)
      val (buf, pos) = arr.target(value.length * SizeOfFloat)
      encodeFloatArrayTo(buf, pos, value)
      arr.flush(buf)
    }

    def encodeDoubleArray(arr: ArrayOutput[Byte], value: Array[Double]): Unit = {
      encodeInt(arr, value.length)
      val (buf, pos) = arr.target(value.length * SizeOfDouble)
      encodeDoubleArrayTo(buf, pos, value)
      arr.flush(buf)
    }

    def encodeCharArray(arr: ArrayOutput[Byte], value: Array[Char]): Unit = {
      encodeInt(arr, value.length)
      val (buf, pos) = arr.target(value.length * SizeOfChar)
      encodeCharArrayTo(buf, pos, value)
      arr.flush(buf)
    }

    def encodeBooleanArray(arr: ArrayOutput[Byte], value: Array[Boolean]): Unit = {
      encodeInt(arr, value.length)
      val (buf, pos) = arr.target(value.length * SizeOfBoolean)
      encodeBooleanArrayTo(buf, pos, value)
      arr.flush(buf)
    }

    def encodeByteArrayTo(arr: Array[Byte], i: Int, value: Array[Byte]): Int = {
      val srcOffset = UnsafeMemory.byteArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i, value.length * SizeOfByte)
      i + value.length * SizeOfByte
    }

    def encodeShortArrayTo(arr: Array[Byte], i: Int, value: Array[Short]): Int = {
      val srcOffset = UnsafeMemory.shortArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i, value.length * SizeOfShort)
      i + value.length * SizeOfShort
    }

    def encodeIntArrayTo(arr: Array[Byte], i: Int, value: Array[Int]): Int = {
      val srcOffset = UnsafeMemory.intArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i, value.length * SizeOfInt)
      i + value.length * SizeOfInt
    }

    def encodeLongArrayTo(arr: Array[Byte], i: Int, value: Array[Long]): Int = {
      val srcOffset = UnsafeMemory.longArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i, value.length * SizeOfLong)
      i + value.length * SizeOfLong
    }

    def encodeFloatArrayTo(arr: Array[Byte], i: Int, value: Array[Float]): Int = {
      val srcOffset = UnsafeMemory.floatArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i, value.length * SizeOfFloat)
      i + value.length * SizeOfFloat
    }

    def encodeDoubleArrayTo(arr: Array[Byte], i: Int, value: Array[Double]): Int = {
      val srcOffset = UnsafeMemory.doubleArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i, value.length * SizeOfDouble)
      i + value.length * SizeOfDouble
    }

    def encodeCharArrayTo(arr: Array[Byte], i: Int, value: Array[Char]): Int = {
      val srcOffset = UnsafeMemory.charArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i, value.length * SizeOfChar)
      i + value.length * SizeOfChar
    }

    def encodeBooleanArrayTo(arr: Array[Byte], i: Int, value: Array[Boolean]): Int = {
      val srcOffset = UnsafeMemory.booleanArrayOffset
      val destOffset = UnsafeMemory.byteArrayOffset
      UnsafeMemory.unsafe.copyMemory(value, srcOffset, arr, destOffset + i, value.length * SizeOfBoolean)
      i + value.length * SizeOfBoolean
    }
  }

  object UnsafeMemory {
    import sun.misc.Unsafe

    private[binary] val unsafe: Unsafe =
      scala.concurrent.util.Unsafe.instance

    private[binary] val byteArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Byte]])
    private[binary] val shortArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Short]])
    private[binary] val intArrayOffset: Long  = unsafe.arrayBaseOffset(classOf[Array[Int]])
    private[binary] val longArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Long]])
    private[binary] val floatArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Float]])
    private[binary] val doubleArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Double]])
    private[binary] val charArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Char]])
    private[binary] val booleanArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Boolean]])

    def putInt(arr: Array[Byte], i: Int, value: Int): Unit = {
      unsafe.putInt(arr, byteArrayOffset + i, value)
    }

    def getInt(arr: Array[Byte], i: Int): Int = {
      unsafe.getInt(arr, byteArrayOffset + i)
    }
  }

}
