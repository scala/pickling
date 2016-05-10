package scala.pickling.binary.list.int.custom.pickler.implicitly.selection

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, binary._

import scala.reflect.runtime.universe._
import scala.collection.mutable.ListBuffer

class BinaryListIntCustomTest extends FunSuite {
  test("main") {
    val lst = (1 to 10).toList

    // Note: Previously list picklers were generated from scratch as `::`.  Now we actually provide list picklers, so this test
    //       is less useful.
    implicit def genListPickler(implicit format: PickleFormat): HandwrittenListIntPicklerUnpickler = new HandwrittenListIntPicklerUnpickler
    class HandwrittenListIntPicklerUnpickler(implicit val format: PickleFormat) extends Pickler[::[Int]] with Unpickler[::[Int]] {
      def pickle(picklee: ::[Int], builder: PBuilder): Unit = {
        builder.beginEntry(picklee, tag)
        val arr = picklee.toArray
        val length = arr.length
        builder.beginCollection(arr.length)
        builder.hintElidedType(FastTypeTag.Int)
        builder.pinHints()

        var i: Int = 0
        while (i < length) {
          builder.beginEntry(arr(i), FastTypeTag.Int)
          builder.endEntry()
          i = i + 1
        }

        builder.unpinHints()
        builder.endCollection()
        builder.endEntry()
      }
      def unpickle(tag: String, reader: PReader): Any = {
        val arrReader = reader.beginCollection()
        arrReader.hintElidedType(FastTypeTag.Int)
        arrReader.pinHints()

        val buffer = ListBuffer[Int]()
        val length = arrReader.readLength()
        var i = 0
        while (i < length) {
          arrReader.beginEntry()
          buffer += arrReader.readPrimitive().asInstanceOf[Int]
          arrReader.endEntry()
          i = i + 1
        }

        arrReader.unpinHints()
        arrReader.endCollection()
        buffer.toList
      }
      def tag: FastTypeTag[::[Int]] = implicitly[FastTypeTag[::[Int]]]
    }

    val pickle = lst.pickle
    // Since we no longer do runtime delegation to `::` class for pickling lists, the custom pickler is no longer used.
    //assert(pickle.toString === "BinaryPickle([0,0,0,50,115,99,97,108,97,46,99,111,108,108,101,99,116,105,111,110,46,105,109,109,117,116,97,98,108,101,46,36,99,111,108,111,110,36,99,111,108,111,110,91,115,99,97,108,97,46,73,110,116,93,0,0,0,10,0,0,0,1,0,0,0,2,0,0,0,3,0,0,0,4,0,0,0,5,0,0,0,6,0,0,0,7,0,0,0,8,0,0,0,9,0,0,0,10])")
    assert(pickle.unpickle[List[Int]] === List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }
}
