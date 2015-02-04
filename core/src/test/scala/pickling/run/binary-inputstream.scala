package scala.pickling.test.binary

import org.scalatest.FunSuite

import scala.pickling._, scala.pickling.Defaults._, binary._

import java.io.ByteArrayInputStream

trait Person[T] {
  val name: String
  val randNums: Array[T]
}

case class PersonInt(name: String, randNums: Array[Int]) extends Person[Int]
case class PersonByte(name: String, randNums: Array[Byte]) extends Person[Byte]
case class PersonShort(name: String, randNums: Array[Short]) extends Person[Short]
case class PersonChar(name: String, randNums: Array[Char]) extends Person[Char]
case class PersonBoolean(name: String, randNums: Array[Boolean]) extends Person[Boolean]

class BinaryInputStreamReaderTest extends FunSuite {
  def mkString[T](obj: Person[T]): String =
    s"Person(${obj.name}, ${obj.randNums.mkString("[", ",", "]")})"

  test("Array[Int]") {
    val arr = Array[Int](30, 31)

    val pickle: BinaryPickle = arr.pickle
    assert(pickle.value.mkString("[", ",", "]") === "[0,0,0,22,115,99,97,108,97,46,65,114,114,97,121,91,115,99,97,108,97,46,73,110,116,93,0,0,0,2,30,0,0,0,31,0,0,0]")

    val streamPickle = BinaryPickle(new ByteArrayInputStream(pickle.value))
    val readArr = streamPickle.unpickle[Array[Int]]
    assert(readArr.mkString("[", ",", "]") === "[30,31]")
  }

  // Byte, Short, Char, Int, Long, Float, Double

  def testPerson[T, U <: Person[T] : FastTypeTag](obj: U)(implicit p: Pickler[U], u: Unpickler[U]): Unit = {
    val pickle       = obj.pickle
    val streamPickle = BinaryPickle(new ByteArrayInputStream(pickle.value))
    val readObj      = streamPickle.unpickle[U]
    assert(mkString(obj) == mkString(readObj))
  }

  test("case class 1") {
    testPerson[Byte, PersonByte](PersonByte("James", (1 to 200).map(_.toByte).toArray))
  }

  test("case class 2") {
    testPerson[Short, PersonShort](PersonShort("James", (1 to 200).map(_.toShort).toArray))
  }

  test("case class 3") {
    testPerson[Char, PersonChar](PersonChar("James", (1 to 200).map(_.toChar).toArray))
  }

  test("case class 4") {
    testPerson[Int, PersonInt](PersonInt("James", (1 to 200).toArray))
  }

  test("case class 5") {
  	val bools = (1 to 200).map(x => scala.util.Random.nextBoolean())
    testPerson[Boolean, PersonBoolean](PersonBoolean("James", bools.toArray))
  }

  test("end of stream") {
    val obj1 = Employee("James", 30)
    val obj2 = Employee("Jim", 40)

    val output = new ByteArrayOutput
    obj1.pickleTo(output)
    obj2.pickleTo(output)

    val streamPickle = BinaryPickle(new ByteArrayInputStream(output.result))
    val readObj1     = streamPickle.unpickle[Employee]
    val readObj2     = streamPickle.unpickle[Employee]
    try {
      streamPickle.unpickle[Employee]
      assert(false, "EndOfStreamException not thrown")
    } catch {
      case _: EndOfStreamException =>
        /* expected */
      case _: java.io.EOFException =>
        /* expected */
    } finally {
      assert(obj1.toString == readObj1.toString)
      assert(obj2.toString == readObj2.toString)
    }
  }
}
