package scala.pickling.bytebuffers

import scala.pickling._, scala.pickling.Defaults._, binary._
import java.nio.ByteBuffer
import org.scalacheck.{Properties, Prop, Gen}
import org.scalatest.FunSuite

object Primitives extends Properties("bytebuffer primitive tests") {
  
  def roundTrip[T: Pickler: Unpickler: FastTypeTag](obj: T): Boolean = {
    try {
      val buf = ByteBuffer.allocate(1024)
      obj.pickleTo(buf)
      buf.rewind
      val p = BinaryPickle(buf)
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
  
}

