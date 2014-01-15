package scala.pickling.binary.util

import org.scalatest.FunSuite
import scala.pickling._
import binary.Util

class BinaryUtilTest extends FunSuite {

  val len = 128
  val rand = new scala.util.Random()

  test("sizeOf") {
    assert(Util.SizeOfByte === 1)
    assert(Util.SizeOfShort === 2)
    assert(Util.SizeOfInt === 4)
    assert(Util.SizeOfLong === 8)
    assert(Util.SizeOfFloat === 4)
    assert(Util.SizeOfDouble === 8)
    assert(Util.SizeOfChar === 2)
    assert(Util.SizeOfBoolean === 1)
  }

  test("byte") {
    val arr = new ByteArrayOutput(Util.SizeOfByte)
	val data = rand.nextInt().toByte
	Util.encodeByte(arr, data)
	assert(arr.result()(0) === data)
  }

  test("short") {
    val arr = new ByteArrayOutput(Util.SizeOfShort)
	val data = rand.nextInt().toShort
	Util.encodeShort(arr, data)
	assert(Util.decodeShortFrom(arr.result(), 0) === data)
  }

  test("int") {
    val arr = new ByteArrayOutput(Util.SizeOfInt)
	val data = rand.nextInt()
	Util.encodeInt(arr, data)
	assert(Util.decodeIntFrom(arr.result(), 0) === data)
  }

  test("long") {
    val arr = new ByteArrayOutput(Util.SizeOfLong)
	val data = rand.nextLong()
	Util.encodeLong(arr, data)
	assert(Util.decodeLongFrom(arr.result(), 0) === data)
  }

  /* Int and Long are used instead.
  test("float") {
    val arr = new ByteArrayOutput(Util.SizeOfFloat)
	val data = rand.nextFloat()
	Util.encodeFloat(arr, data)
	assert(Util.decodeFloatFrom(arr.result(), 0) === data)
  }

  test("double") {
    val arr = new ByteArrayOutput(Util.SizeOfDouble)
	val data = rand.nextDouble()
	Util.encodeDouble(arr, data)
	assert(Util.decodeDoubleFrom(arr.result(), 0) === data)
  }
  */

  test("char") {
    val arr = new ByteArrayOutput(Util.SizeOfChar)
	val data = rand.nextInt().toChar
	Util.encodeChar(arr, data)
	assert(Util.decodeCharFrom(arr.result(), 0) === data)
  }

  test("string") {
    val data = rand.nextString(len)
    val arr = new ByteArrayOutput(data.getBytes("UTF-8").length+Util.SizeOfInt)
	Util.encodeString(arr, data)
	assert(Util.decodeStringFrom(arr.result(), 0)._1 === data)
  }

  test("byte-array") {
    val arr = new ByteArrayOutput(Util.SizeOfByte*len+Util.SizeOfInt)
	val data = Array.ofDim[Byte](len).map(_ => rand.nextInt().toByte)
	Util.encodeByteArray(arr, data)
	assert(Util.decodeByteArrayFrom(arr.result(), 0)._1 === data)
  }

  test("short-array") {
    val arr = new ByteArrayOutput(Util.SizeOfShort*len+Util.SizeOfInt)
	val data = Array.ofDim[Short](len).map(_ => rand.nextInt().toShort)
	Util.encodeShortArray(arr, data)
	assert(Util.decodeShortArrayFrom(arr.result(), 0)._1 === data)
  }

  test("int-array") {
    val arr = new ByteArrayOutput(Util.SizeOfInt*len+Util.SizeOfInt)
	val data = Array.ofDim[Int](len).map(_ => rand.nextInt())
	Util.encodeIntArray(arr, data)
	assert(Util.decodeIntArrayFrom(arr.result(), 0)._1 === data)
  }

  test("long-array") {
    val arr = new ByteArrayOutput(Util.SizeOfLong*len+Util.SizeOfInt)
	val data = Array.ofDim[Long](len).map(_ => rand.nextLong())
	Util.encodeLongArray(arr, data)
	assert(Util.decodeLongArrayFrom(arr.result(), 0)._1 === data)
  }

  test("float-array") {
    val arr = new ByteArrayOutput(Util.SizeOfFloat*len+Util.SizeOfInt)
	val data = Array.ofDim[Float](len).map(_ => rand.nextFloat())
	Util.encodeFloatArray(arr, data)
	assert(Util.decodeFloatArrayFrom(arr.result(), 0)._1 === data)
  }

  test("double-array") {
    val arr = new ByteArrayOutput(Util.SizeOfDouble*len+Util.SizeOfInt)
	val data = Array.ofDim[Double](len).map(_ => rand.nextDouble())
	Util.encodeDoubleArray(arr, data)
	assert(Util.decodeDoubleArrayFrom(arr.result(), 0)._1 === data)
  }

  test("char-array") {
    val arr = new ByteArrayOutput(Util.SizeOfChar*len+Util.SizeOfInt)
	val data = Array.ofDim[Char](len).map(_ => rand.nextInt().toChar)
	Util.encodeCharArray(arr, data)
	assert(Util.decodeCharArrayFrom(arr.result(), 0)._1 === data)
  }

  test("boolean-array") {
    val arr = new ByteArrayOutput(Util.SizeOfBoolean*len+Util.SizeOfInt)
	val data = Array.ofDim[Boolean](len).map(_ => rand.nextBoolean())
	Util.encodeBooleanArray(arr, data)
	assert(Util.decodeBooleanArrayFrom(arr.result(), 0)._1 === data)
  }
}
