package scala.pickling.runtime

import org.scalatest.FunSuite
import scala.pickling._
import Defaults._
import json._

class AutoRegisteringTest extends FunSuite {

  case class Foo[T](value: T)
  import scala.pickling.internal.currentRuntime

  test("picklers should autoregister themselves when they are instantiated") {

    val pickler = implicitly[Pickler[Foo[List[(Int, String)]]]]
    val key = pickler.tag.key
    val x = currentRuntime.picklers.lookupPickler(key)
    assert(x.get === pickler)

  }

  test("unpicklers should autoregister themselves when they are instantiated") {

    val unpickler = implicitly[Unpickler[Foo[List[(Int, String)]]]]
    val key = unpickler.tag.key
    val y = currentRuntime.picklers.lookupUnpickler(key)
    assert(y.get === unpickler)

  }

}

