package scala.pickling.test

import scala.pickling._, Defaults._
import org.scalatest.FunSuite


sealed trait Fruit
final case class Apple(kind: String) extends Fruit
final case class Orange(ripeness: String) extends Fruit

class SealedTraitHierarchyTest extends FunSuite {

  // TODO: this should not need a FastTypeTag
  def unpickleWrapper[T](s: String)(implicit unpickler1: Unpickler[T], tag: FastTypeTag[T]): T = {
    import scala.pickling.json._
    JSONPickle(s).unpickle[T]
  }

  def pickleWrapper[T](s: T)(implicit pickler1: Pickler[T], tag1: FastTypeTag[T]): String = {
    import scala.pickling.json._
    s.pickle.value
  }

  test("main") {
    val apple = Apple("Fuji")
    val appleString = pickleWrapper[Fruit](apple)
    assert(unpickleWrapper[Fruit](appleString) == apple)
    assert(unpickleWrapper[Apple](appleString) == apple)
  }

}
