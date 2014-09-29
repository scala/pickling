package scala.pickling.fastbinary
package test

import org.scalatest.FunSuite

class ByteArrayBuilderTest extends FunSuite {

  def bufferWithEmptyArray(): ByteArrayBuilder = {
    val empty = Array.ofDim[Byte](0)
    new ByteArrayBuffer(empty)
  }

  def bufferOfDim(size: Int): ByteArrayBuilder =
    new ByteArrayBuffer(Array.ofDim[Byte](size))

  test("buffer with empty array: isFull") {
    val builder: ByteArrayBuilder = bufferWithEmptyArray()
    assert(builder.isFull == false)
  }

  test("buffer with empty array: +=") {
    val builder: ByteArrayBuilder = bufferWithEmptyArray()
    builder += 5
    val a = builder.toArray
    assert(a.length == 1)
    assert(a(0) == 5)
  }

  test("buffer with empty array: toArray") {
    val builder: ByteArrayBuilder = bufferWithEmptyArray()
    assert(builder.toArray.length == 0)
  }

  test("buffer with non-empty array") {
    val builder: ByteArrayBuilder = bufferOfDim(1)
    builder += 5
    val a = builder.toArray
    assert(a.length == 2)
    assert(a(0) == 0)
    assert(a(1) == 5)
  }

  test("byte array, empty: toArray") {
    val builder: ByteArrayBuilder = new ByteArray(4)
    val a = builder.toArray
    assert(a.length == 0)
  }

  test("byte array, non-empty: toArray") {
    val builder: ByteArrayBuilder = new ByteArray(4)
    builder += 1
    builder += 2
    val a = builder.toArray
    assert(a.length == 2)
    assert(a(0) == 1)
    assert(a(1) == 2)
  }

  test("byte array, non-empty: checkpoint 1") {
    val builder: ByteArrayBuilder = new ByteArray(4)
    builder += 1
    builder += 2
    val ok = builder.checkpoint()
    assert(ok == true)
    val a = builder.toArray
    assert(a.length == 0)
  }

  test("byte array, non-empty: checkpoint 2") {
    val builder: ByteArrayBuilder = new ByteArray(4)
    val ok = builder.checkpoint()
    assert(ok == true)
    builder += 1
    builder += 2
    val a = builder.toArray
    assert(a.length == 2)
  }

  test("byte array, non-empty: checkpoint 3") {
    val builder: ByteArrayBuilder = new ByteArray(4)
    val ok1 = builder.checkpoint()
    assert(ok1 == true)
    builder += 1
    builder += 2
    val a1 = builder.toArray
    assert(a1.length == 2)
    assert(a1(0) == 1)
    val ok2 = builder.checkpoint()
    assert(ok2 == true)
    builder += 3
    builder += 4
    val a2 = builder.toArray
    assert(a2.length == 2)
    assert(a2(0) == 3)
  }

  test("byte array: target with overflow") {
    val builder: ByteArrayBuilder = new ByteArray(1)
    val ok1 = builder.checkpoint()
    assert(ok1 == true)
    try {
      builder.target(2)
      assert(false)
    } catch {
      case e: IllegalArgumentException =>
        assert(true)
    }
  }

  test("byte array: target without overflow") {
    val builder: ByteArrayBuilder = new ByteArray(2)
    val ok1 = builder.checkpoint()
    assert(ok1 == true)
    builder += 1
    try {
      builder.target(1)
      assert(true)
    } catch {
      case e: IllegalArgumentException =>
        assert(false)
    }
  }
}
