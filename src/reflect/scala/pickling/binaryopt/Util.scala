package binaryopt {
  import collection.mutable.Buffer
  import UnsafeMemory._

  object Util {

    def copy(target: Array[Byte], pos: Int, arr: Array[Byte]): Unit = {
      // def copy(src: Object, srcPos: Int, dest: Object, destPos: Int, length: Int)
      Array.copy(arr, 0, target, pos, arr.length)
    }

    /** Returns decoded Int plus next "readable" position in target array.
     */
    def decodeIntFrom(arr: Array[Byte], i: Int): (Int, Int) = {
      val fst = (arr(i) << 24).toInt
      val snd = ((arr(i+1) << 16) & 0x00FFFFFF).toInt
      val thrd = ((arr(i+2) << 8) & 0x0000FFFF).toInt
      val frth = (arr(i+3) & 0x000000FF).toInt
      (fst | snd | thrd | frth, i+4)
    }

    def fastdecodeIntFrom(arr: Array[Byte], i: Int): (Int, Int) = {
      val fst = (getInt(arr, i) << 24).toInt
      val snd = ((getInt(arr, i+1) << 16) & 0x00FFFFFF).toInt
      val thrd = ((getInt(arr, i+2) << 8) & 0x0000FFFF).toInt
      val frth = (getInt(arr, i+3) & 0x000000FF).toInt
      (fst | snd | thrd | frth, i+4)
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

    def decodeBooleanFrom(arr: Array[Byte], i: Int): (Boolean, Int) = {
      val res = arr(i) != 0
      (res, i + 1)
    }

    def fastdecodeBooleanFrom(arr: Array[Byte], i: Int): (Boolean, Int) = {
      val res = getInt(arr, i) != 0
      (res, i + 1)
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
      val (len, _) = decodeIntFrom(arr, i)
      val bytes = Array.ofDim[Byte](len)
      Array.copy(arr, i + 4, bytes, 0, len)
      (new String(bytes, "UTF-8"), i + 4 + len)
    }

    def fastdecodeStringFrom(arr: Array[Byte], i: Int): (String, Int) = {
      val (len, _) = fastdecodeIntFrom(arr, i)
      val bytes = Array.ofDim[Byte](len)
      Array.copy(arr, i + 4, bytes, 0, len)
      (new String(bytes, "UTF-8"), i + 4 + len)
    }

    def encodeStringTo(arr: Array[Byte], i: Int, value: String): Int = {
      val bytes = value.getBytes("UTF-8")
      // encode length
      val next = encodeIntTo(arr, i, bytes.length)
      // encode bytes of string at `next` position
      Util.copy(arr, next, bytes)
      next + bytes.length
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

  }

  object UnsafeMemory {
    import sun.misc.Unsafe

    private val unsafe: Unsafe =// Unsafe.getUnsafe()
      scala.concurrent.util.Unsafe.instance

    private val byteArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Byte]])

    def putInt(buffer: Array[Byte], pos: Int, value: Int): Unit = {
      unsafe.putInt(buffer, byteArrayOffset + pos, value)
    }

    def getInt(buffer: Array[Byte], pos: Int): Int = {
      unsafe.getInt(buffer, byteArrayOffset + pos)
    }
  }

}
