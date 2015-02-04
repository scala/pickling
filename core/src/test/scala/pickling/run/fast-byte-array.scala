package scala.pickling.fastbinary
package test

import org.scalatest.FunSuite
import org.scalacheck.{Properties, Prop, Gen}

import scala.pickling._, scala.pickling.Defaults._, binary._

import org.scalatest.FunSuite

class FastArrayOutputTest extends FunSuite {

    test("allocating chunks") {
        //allocate a dummy to grab the preallocated array
        val dummy = new FastByteArrayOutput
        val out = new FastByteArrayOutput
        val size = 10000
        for (_ <- 1 to size) out.putByte(0x0f)
        val result = out.result
        assert(result.length == size)
        assert(result.forall( _ == 0x0f))
        //release the preallocated array
        dummy.result
    }

}

object Primitives extends Properties("fast byte array primitive tests") {
  
  def roundTrip[T: Pickler: Unpickler: FastTypeTag](obj: T): Boolean = {
    try {
      //val dummy = new FastByteArrayOutput
      val out = new FastByteArrayOutput
      obj.pickleTo(out)
      //dummy.result
      val p = BinaryPickle(out.result)
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

