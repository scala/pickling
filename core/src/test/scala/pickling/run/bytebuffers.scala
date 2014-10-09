package scala.pickling.bytebuffers

import scala.pickling._
import binary._
import java.nio.ByteBuffer
import org.scalacheck.{Properties, Prop, Gen}
import org.scalatest.FunSuite

object Primitives extends Properties("bytebuffer primitive tests") {
  
  def roundTrip[T: SPickler: Unpickler: FastTypeTag](obj: T): Boolean = {
    try {
      val out = new ByteBufferOutput(ByteBuffer.allocate(512))
      val builder = pickleFormat.createBuilder(out)
      obj.pickleInto(builder)
      val buf = out.buffer
      buf.rewind
      val p = new BinaryInputPickle(new ByteBufferInput(buf))
      val up = p.unpickle[T]
      obj == up
    } catch {
      case e: Throwable =>
        Console.err.println(e)
        e.printStackTrace
        throw e
    }
  }
  
  property("Int") = Prop forAll { (i: Int) => roundTrip[Int](i) }
  property("Double") = Prop forAll { (d: Double) => roundTrip[Double](d) }
  property("Long") = Prop forAll { (l: Long) => roundTrip[Long](l) }
  property("Char") = Prop forAll { (c: Char) => roundTrip[Char](c) }
  property("Float") = Prop forAll { (f: Float) => roundTrip[Float](f) }
  property("Boolean") = Prop forAll { (b: Boolean) => roundTrip[Boolean](b) }
  property("Short") = Prop forAll { (s: Short) => roundTrip[Short](s) }
  property("Byte") = Prop forAll { (b: Byte) => roundTrip[Byte](b) }
  property("String") = Prop forAll { (s: String) => roundTrip[String](s) }
  property("(Int, String)") = Prop forAll { (p: (Int, String)) => roundTrip[(Int,String)](p) }

}

class ByteBufferTest extends FunSuite {

  test("lookahead") {
    val buf = ByteBuffer.allocate(32)
    val out = new ByteBufferOutput(buf)
    out.putInt(0x12345678)
    buf.rewind
    val in = new ByteBufferInput(buf)
    val b = in.getByte
    in.setLookahead(b)
    val res = in.getIntWithLookahead
    res == 0x12345678
  }
  
  test("reallocation") {
    val obj = (0 until 500).toArray
    val buf = ByteBuffer.allocate(12)
    val out = new ByteBufferOutput(buf)
    val builder = pickleFormat.createBuilder(out)
    obj.pickleInto(builder)
    val buf2 = out.buffer
    buf2.rewind
    val p = new BinaryInputPickle(new ByteBufferInput(buf2))
    val up = p.unpickle[Array[Int]]
    obj == up
  }

}

