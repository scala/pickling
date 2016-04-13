package scala.pickling.runtime

import org.scalatest.FunSuite
import scala.pickling._
import Defaults._
import json._

class AutoRegistrationTest extends FunSuite {

  case class Foo[T](value: T)

  test("picklers should autoregister themselves when they are instantiated") {

    val pickler = implicitly[Pickler[Foo[List[String]]]]
    val key = pickler.tag.key
    val x = scala.pickling.internal.currentRuntime.picklers.lookupPickler(key)
    assert(x.get === pickler)

  }

  test("unpicklers should autoregister themselves when they are instantiated") {

    val unpickler = implicitly[Unpickler[Foo[List[String]]]]
    val key = unpickler.tag.key
    val y = scala.pickling.internal.currentRuntime.picklers.lookupUnpickler(key)
    assert(y.get === unpickler)

  }

}

