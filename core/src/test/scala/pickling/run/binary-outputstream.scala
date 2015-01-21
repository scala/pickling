package scala.pickling.test.binary

import org.scalatest.FunSuite

import java.io.{OutputStream, ByteArrayOutputStream, ByteArrayInputStream}

import scala.pickling._, scala.pickling.Defaults._, binary._

class BinaryOutputStreamTest extends FunSuite {
  test("pickle to OutputStream") {
    val obj1 = Employee("James", 30)
    val obj2 = Employee("Jim", 40)

    val stream = new ByteArrayOutputStream
    val output = new StreamOutput(stream)
    obj1.pickleTo(output)
    obj2.pickleTo(output)

    val streamPickle = BinaryPickle(new ByteArrayInputStream(stream.toByteArray))
    val readObj1     = streamPickle.unpickle[Employee]
    val readObj2     = streamPickle.unpickle[Employee]

    assert(obj1.toString == readObj1.toString)
    assert(obj2.toString == readObj2.toString)
  }
}
