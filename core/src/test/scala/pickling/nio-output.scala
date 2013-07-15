package pickling.test.nio

import java.nio.ByteBuffer

import scala.pickling._
import scala.pickling.binary._
import scala.pickling.io.{ByteBufferOutput, ByteBufferInput}

import org.scalatest.FunSuite

case class Person(name: String)

class NIOByteBufferTest extends FunSuite {
  val p = Person("James")
  var bytes: Array[Byte] = null

  def checkUnpickle(buf: ByteBuffer) = {
    buf.flip()
    bytes = Array.ofDim[Byte](buf.remaining)
    buf.get(bytes)

    // unpickle from bytes array
    val bpickle: BinaryPickle = bytes
    val up = bpickle.unpickle[Person]
    assert(up.toString == "Person(James)")
  }

  test("output") {
    val buf = ByteBuffer.allocate(1024)
    val out = new ByteBufferOutput(buf)

    val builder = pickleFormat.createBuilder(out)
    p.pickleInto(builder)
    clearPicklees()

    checkUnpickle(buf)
  }

  test("pickleTo") {
    val buf = ByteBuffer.allocate(1024)
    val out = new ByteBufferOutput(buf)

    p.pickleTo(out)

    checkUnpickle(buf)
  }

  test("input") {
    val buf = ByteBuffer.wrap(bytes)
    val input = new ByteBufferInput(buf)

    // now we need to wire this up with a PickleReader
    val reader = pickleFormat.createReader(input, scala.pickling.`package`.currentMirror)
    val up = reader.unpickleTopLevel[Person]
    assert(up.toString == "Person(James)")
  }

  test("unpickleFrom") {
    val buf = ByteBuffer.wrap(bytes)
    val input = new ByteBufferInput(buf)

    val up = pickleFormat.unpickleFrom[Person](input)
    assert(up.toString == "Person(James)")
  }
}
