package scala.pickling.test.sealedtraitstatic

import scala.pickling.{PicklerUnpickler, Pickler, Unpickler, PicklingException}
import scala.pickling.static._
import scala.pickling.json._
import scala.pickling.Defaults._

import org.scalatest.FunSuite


sealed trait Fruit

sealed trait RedOrOrangeFruit extends Fruit
final case class Apple(kind: String) extends RedOrOrangeFruit
final case class Orange(ripeness: String) extends RedOrOrangeFruit
final case class Banana(something: Int) extends Fruit

final case class Cucumber(something: Int) // does not extend Fruit but same shape as Banana

object Fruit {
  implicit val picklerUnpickler = PicklerUnpickler.generate[Banana]
}

class SealedTraitStaticTest extends FunSuite {

  test("main") {
    val apple = Apple("Fuji")
    val appleString = (apple: Fruit).pickle.value
    assert(JSONPickle(appleString).unpickle[Fruit] == apple)
    assert(JSONPickle(appleString).unpickle[Apple] == apple)

    val banana = Banana(42)
    val bananaString = (banana: Fruit).pickle.value
    assert(JSONPickle(bananaString).unpickle[Fruit] == banana)
    assert(JSONPickle(bananaString).unpickle[Banana] == banana)

    // if we are only using static (un)picklers, then the Fruit
    // unpickler should not know a thing about Cucumber.
    try {
      val f = JSONPickle(bananaString.replace("Banana", "Cucumber")).unpickle[Fruit]
      throw new Exception(s"Should have thrown on unpickle but instead parsed $f")
    } catch {
      case PicklingException(message, cause) =>
        if (!message.contains("Cucumber not recognized"))
          throw new Exception(s"Not the expected exception: $message")
    }

    // but since the Banana unpickler doesn't need the type tag as a discriminator,
    // it should accept anything with the same shape ignoring the tag
    val bananaShapedCucumber = JSONPickle(bananaString.replace("Banana", "Cucumber")).unpickle[Banana]
    assert(bananaShapedCucumber == banana)

    // The Apple unpickler isn't shaped the same so we can't unpickle a banana as an apple
    // the exception must be based on shape, not type tag
    try {
      val a = JSONPickle(bananaString).unpickle[Apple]
      throw new Exception(s"Should have thrown on unpickle but instead parsed $a")
    } catch {
      case PicklingException(message, cause) =>
        if (!message.contains("No field 'kind'"))
          throw new Exception(s"Not the expected exception: $message")
    }
  }
}
