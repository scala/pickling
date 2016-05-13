package scala.pickling.generator

import org.scalatest.FunSuite

import scala.pickling.PicklerUnpickler

// Case #1 - Everything sealed and together.
object SimpleAdt {
  import scala.pickling.Defaults._

  // Note: this is needed due to the way we compile/subclass detection works.
  @scala.pickling.directSubclasses(value = Array(classOf[Foo], classOf[Bar]))
  sealed trait Adt
  final case class Foo(x: Int) extends Adt
  final case class Bar(y: String) extends Adt
  implicit val fp2 = {
    implicit val fp = PicklerUnpickler.generate[Foo]
    implicit val fp3 = PicklerUnpickler.generate[Bar]
    PicklerUnpickler.generate[Adt]
  }
}

object NestedAdt {
  import scala.pickling.Defaults._
  @scala.pickling.directSubclasses(value = Array(classOf[Foo], classOf[NestedAdt]))
  sealed trait Adt
  final case class Foo(x: Int) extends Adt
  @scala.pickling.directSubclasses(value = Array(classOf[Bar]))
  sealed trait NestedAdt extends Adt
  final case class Bar(y: String) extends Adt
  implicit val p = {
    implicit val f = PicklerUnpickler.generate[Foo]
    implicit val f2 = {
      implicit val b = PicklerUnpickler.generate[Bar]
      PicklerUnpickler.generate[NestedAdt]
    }
    PicklerUnpickler.generate[Adt]
  }
}



class AdtGeneratorTest  extends FunSuite {
  import scala.pickling.Defaults._
  import scala.pickling.json._
  test("simpleAdt") {
    import SimpleAdt.{Adt, Foo, Bar, fp2}
    val x: Adt = Foo(5)
    val x1: Adt = Bar("hi")
    val y = x.pickle.unpickle[Adt]
    val y1 = x1.pickle.unpickle[Adt]
    assert(x == y)
    assert(x1 == y1)
  }

  test("nestedAdt") {
    import NestedAdt.{Adt, Foo, Bar, p}
    val x: Adt = Foo(5)
    val x1: Adt = Bar("hi")
    val y = x.pickle.unpickle[Adt]
    val y1 = x1.pickle.unpickle[Adt]
    assert(x == y)
    assert(x1 == y1)
  }
}
