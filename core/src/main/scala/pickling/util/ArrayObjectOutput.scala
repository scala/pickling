package scala.pickling.util

import java.io.{ObjectOutput, ObjectInput}
import scala.collection.mutable.ArrayBuffer

/** An `ObjectOutput` that enables access to the written data using array-valued fields.
 */
trait ArrayObjectOutput extends ObjectOutput {
  def booleanArr: Array[Boolean]
  def byteArr: Array[Int]
  def charArr: Array[Int]
  def doubleArr: Array[Double]
  def floatArr: Array[Float]
  def intArr: Array[Int]
  def longArr: Array[Long]
  def shortArr: Array[Int]
  def arrByteArr: Array[Array[Byte]]
  def anyRefArr: Array[Any]
}

class GenObjectInput(booleanIter: Iterator[Boolean],
                     byteIter: Iterator[Int],
                     charIter: Iterator[Int],
                     doubleIter: Iterator[Double],
                     floatIter: Iterator[Float],
                     intIter: Iterator[Int],
                     longIter: Iterator[Long],
                     shortIter: Iterator[Int],
                     arrByteIter: Iterator[Array[Byte]],
                     anyRefIter: Iterator[Any],
                     stringIter: Iterator[String]) extends ObjectInput {
  def readByte(): Byte = byteIter.next().asInstanceOf[Byte]
  def readBoolean(): Boolean = booleanIter.next()
  def readChar(): Char = charIter.next().asInstanceOf[Char]
  def readDouble(): Double = doubleIter.next()
  def readFloat(): Float = floatIter.next()
  def readFully(x1: Array[Byte], x2: Int, x3: Int): Unit = ???
  def readFully(dest: Array[Byte]): Unit = {
    val src = arrByteIter.next()
    java.lang.System.arraycopy(src, 0, dest, 0, dest.length)
  }
  def readInt(): Int = intIter.next()
  def readLine(): String = ???
  def readLong(): Long = longIter.next()
  def readShort(): Short = shortIter.next().asInstanceOf[Short]
  def readUTF(): String = stringIter.next()
  def readUnsignedByte(): Int = ???
  def readUnsignedShort(): Int = ???
  def skipBytes(x1: Int): Int = ???
  def available(): Int = ???
  def close(): Unit = ???
  def read(x1: Array[Byte], x2: Int, x3: Int): Int = ???
  def read(x1: Array[Byte]): Int = ???
  def read(): Int = ???
  def readObject(): AnyRef = anyRefIter.next().asInstanceOf[AnyRef]
  def skip(x1: Long): Long = ???
}

case class GenObjectOutput(
  val booleanArrBuf: ArrayBuffer[Boolean] = new ArrayBuffer[Boolean],
  val byteArrBuf: ArrayBuffer[Int] = new ArrayBuffer[Int], // Byte stored as Int
  val charArrBuf: ArrayBuffer[Int] = new ArrayBuffer[Int],
  val doubleArrBuf: ArrayBuffer[Double] = new ArrayBuffer[Double],
  val floatArrBuf: ArrayBuffer[Float] = new ArrayBuffer[Float],
  val intArrBuf: ArrayBuffer[Int] = new ArrayBuffer[Int],
  val longArrBuf: ArrayBuffer[Long] = new ArrayBuffer[Long],
  val shortArrBuf: ArrayBuffer[Int] = new ArrayBuffer[Int],
  val arrByteArrBuf: ArrayBuffer[Array[Byte]] = new ArrayBuffer[Array[Byte]],
  val anyRefArrBuf: ArrayBuffer[Any] = new ArrayBuffer[Any],
  val stringArrBuf: ArrayBuffer[String] = new ArrayBuffer[String]
  ) extends ObjectOutput {
  def toInput: ObjectInput = new GenObjectInput(
    booleanArrBuf.iterator,
    byteArrBuf.iterator,
    charArrBuf.iterator,
    doubleArrBuf.iterator,
    floatArrBuf.iterator,
    intArrBuf.iterator,
    longArrBuf.iterator,
    shortArrBuf.iterator,
    arrByteArrBuf.iterator,
    anyRefArrBuf.iterator,
    stringArrBuf.iterator)

  // Members declared in java.io.DataOutput
  def writeBoolean(x: Boolean): Unit = { booleanArrBuf += x }
  def writeByte(x: Int): Unit = { byteArrBuf += x }
  def writeBytes(x: String): Unit = ???
  def writeChar(x: Int): Unit = { charArrBuf += x }
  def writeChars(x: String): Unit = ???
  def writeDouble(x: Double): Unit = { doubleArrBuf += x }
  def writeFloat(x: Float): Unit = { floatArrBuf += x }
  def writeInt(x: Int): Unit = { intArrBuf += x }
  def writeLong(x: Long): Unit = { longArrBuf += x }
  def writeShort(x: Int): Unit = { shortArrBuf += x }
  def writeUTF(x: String): Unit = { stringArrBuf += x }

  // Members declared in java.io.ObjectOutput
  def close(): Unit = {}
  def flush(): Unit = ???
  def write(x1: Array[Byte],x2: Int,x3: Int): Unit = ???
  def write(x: Array[Byte]): Unit = { arrByteArrBuf += x }
  def write(x: Int): Unit = ???
  def writeObject(x: Any): Unit = { anyRefArrBuf += x }
}
