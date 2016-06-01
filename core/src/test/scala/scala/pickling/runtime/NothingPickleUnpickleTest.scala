package scala.pickling.runtime

import org.scalatest.FunSuite

import scala.pickling.Defaults._
import scala.pickling.json._

import scala.pickling.AbstractPicklerUnpickler
import scala.pickling.PicklingErrors.NothingIsNotUnpicklable

class NothingPickleUnpickleTest extends FunSuite {

  val nothingPU = implicitly[AbstractPicklerUnpickler[Nothing]]

  // `pickle` cannot simply be called, only test `unpickle`
  test("unpickle `Nothing` throws an exception") {
    intercept[NothingIsNotUnpicklable.type] {
      val reader = pickleFormat.createReader(JSONPickle("{}"))
      nothingPU.unpickle("", reader)
    }
  }

}
