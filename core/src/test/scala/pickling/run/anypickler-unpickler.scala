package scala.pickling.runtime

import org.scalatest.FunSuite
import scala.pickling._

import scala.pickling.json.JsonFormats
import scala.pickling.pickler.AllPicklers
import scala.pickling.json.JSONPickle
import scala.pickling.internal.HybridRuntime

abstract class PicklingLogic extends Ops with AllPicklers with JsonFormats

object PicklingProtocol extends {
  val ignoreMe = internal.replaceRuntime(new HybridRuntime)
} with PicklingLogic {
  implicit val so = static.StaticOnly
}

case class Box[T](value: T)

class AnyPicklerUnpickler extends FunSuite {

  import PicklingProtocol._

  test("AnyPickler should work") {

    val b = Box(List(1,2,3,4,5))
    val containerPickler = implicitly[Pickler[List[Int]]]
    val pickled = b.pickle.value

    import scala.pickling.pickler.AnyPickler
    val pickled2 = AnyPickler.pickle(b, pickleFormat.createBuilder())

    println(pickled)
    println(pickled2)

    assert(pickled === pickled2)

  }

  test("AnyUnpickler should work") {

    val b = Box(List(1,2,3,4,5))
    val containerPickler = implicitly[Pickler[List[Int]]]
    val pickled = b.pickle.value

    import scala.pickling.pickler.AnyUnpickler
    val format = implicitly[PickleFormat]
    val reader = pickleFormat.createReader(JSONPickle(pickled))
    val tag1 = reader.beginEntry()
    val unpickled = AnyUnpickler.unpickle(tag1, reader)
    reader.endEntry()

    println(unpickled)

    assert(unpickled === b)

  }

}
