package scala.pickling.binary

import sun.misc.Unsafe

object UnsafeMemory {
  private[pickling] val unsafe: Unsafe = {
    val fields = classOf[Unsafe].getDeclaredFields()
    val unsafes = for (field <- fields; if field.getType == classOf[Unsafe]) yield {
      field.setAccessible(true)
      field.get(null).asInstanceOf[Unsafe]
    }
    unsafes.headOption.getOrElse(
      throw new IllegalStateException("Can't find instance of sun.misc.Unsafe"))
  }
  private[pickling] val byteArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Byte]])
  private[pickling] val shortArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Short]])
  private[pickling] val intArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Int]])
  private[pickling] val longArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Long]])
  private[pickling] val floatArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Float]])
  private[pickling] val doubleArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Double]])
  private[pickling] val charArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Char]])
  private[pickling] val booleanArrayOffset: Long = unsafe.arrayBaseOffset(classOf[Array[Boolean]])
  def putInt(arr: Array[Byte], i: Int, value: Int): Unit = {
    unsafe.putInt(arr, byteArrayOffset + i, value)
  }
  def getInt(arr: Array[Byte], i: Int): Int = {
    unsafe.getInt(arr, byteArrayOffset + i)
  }
}
