package scala.pickling.pickler

import org.scalatest.FunSuite
import scala.pickling._, Defaults._, static._, json._

class DoublePicklerUnpicklerGeneration extends FunSuite {

  import scala.pickling.internal.currentRuntime

  test("generation of pickler should return an already generated pickler") {

    // This should get the already generated pickler
    // which is already stored in the mapPickler
    val pickler = implicitly[Pickler[List[String]]]
    val key = pickler.tag.key
    val alreadyGenPickler = currentRuntime.picklers.lookupPickler(key)

    assert(alreadyGenPickler.get === pickler)

  }

  test("generation of unpickler should return an already generated unpickler") {

    val unpickler = implicitly[Unpickler[List[String]]]
    val key = unpickler.tag.key
    val alreadyGenUnpickler = currentRuntime.picklers.lookupUnpickler(key)

    assert(alreadyGenUnpickler.get === unpickler)

  }

}
