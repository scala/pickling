package scala.pickling.test.binary

import org.scalatest.FunSuite

import java.io.{OutputStream, ByteArrayOutputStream, ByteArrayInputStream}

import scala.pickling._
import binary._

class OutputStreamOutput(out: OutputStream) extends ArrayOutput[Byte] {

  def result(): Array[Byte] =
    null

  def +=(obj: Byte) =
    out.write(obj.asInstanceOf[Int])

  def put(obj: Array[Byte]): this.type = {
    out.write(obj)
    this
  }
}

class BinaryOutputStreamTest extends FunSuite {
  test("pickle to OutputStream") {
    val obj1 = Employee("James", 30)
    val obj2 = Employee("Jim", 40)

    val stream = new ByteArrayOutputStream
    val output = new OutputStreamOutput(stream)
    obj1.pickleTo(output)
    obj2.pickleTo(output)

    val streamPickle = BinaryPickleStream(new ByteArrayInputStream(stream.toByteArray))
    val readObj1     = streamPickle.unpickle[Employee]
    val readObj2     = streamPickle.unpickle[Employee]

    assert(obj1.toString == readObj1.toString)
    assert(obj2.toString == readObj2.toString)
  }
}
