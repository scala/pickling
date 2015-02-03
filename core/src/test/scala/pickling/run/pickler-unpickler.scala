package scala.pickling.picklerunpickler

import org.scalatest.FunSuite
import scala.pickling._, functions._, json._
import static.StaticOnly

class CakeHasSPicklerUnpickler extends pickler.PrimitivePicklers {
  // be sure SPicklerUnpickler exists for primitives,
  // mostly we just want to know that this compiles
  assert(implicitly[SPicklerUnpickler[Int]] ne null)
  assert(implicitly[SPicklerUnpickler[String]] ne null)

  // we could check more of the cake traits, but at present
  // there's no real need because SPicklerUnpickler is just
  // a type alias and impossible to mess up (ha).
  // If SPicklerUnpickler were a trait, you'd need to be
  // sure all the cake picklers used it.
}

class PicklerUnpicklerTest extends FunSuite {
  test("main") {
    new CakeHasSPicklerUnpickler()
  }
}
