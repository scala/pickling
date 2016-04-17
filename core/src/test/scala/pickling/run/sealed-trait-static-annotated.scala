package scala.pickling.test.sealedtraitstaticannotated

import scala.pickling._
import scala.pickling.static._
import scala.pickling.json._
import Defaults.{ stringPickler, intPickler, refUnpickler, nullPickler }
import Defaults.{ pickleOps, unpickleOps }

import org.scalatest.FunSuite

// we are skipping RedOrOrangeFruit here in the annotation
@directSubclasses(Array(classOf[Banana]))
trait Fruit

object Banana {
  implicit val picklerUnpickler = Defaults.genPickler[Banana]
}

// this is BEFORE the subtypes below so directKnownSubclasses probably
// won't work and this would break without the directSubclasses annotation.
object Fruit {
  implicit val picklerUnpickler = Defaults.genPickler[Fruit]
}

sealed trait RedOrOrangeFruit extends Fruit
final case class Apple(kind: String) extends RedOrOrangeFruit
final case class Orange(ripeness: String) extends RedOrOrangeFruit
final case class Banana(something: Int) extends Fruit

final case class Cucumber(something: Int) // does not extend Fruit

final case class Pumpkin(kind: String)

class SealedTraitStaticAnnotatedTest extends FunSuite {

  test("main") {
    // we should be able to pickle and unpickle a Banana, using either
    // Fruit or Banana unpickler
    val banana = Banana(42)
    val bananaString = (banana: Fruit).pickle.value
    assert(JSONPickle(bananaString).unpickle[Fruit] == banana)
    assert(JSONPickle(bananaString).unpickle[Banana] == banana)

    // we did not list apple in directSubclasses so it should be
    // as if Apple doesn't exist
    val apple = Apple("Fuji")
    try {
      val pickled = (apple: Fruit).pickle.value
      throw new Exception(s"We should have failed to pickle Apple but we pickled as: $pickled")
    } catch {
      case PicklingException(message, cause) =>
        if (!message.contains("Apple not recognized"))
          throw new Exception(s"Not the expected exception: $message")
    }

    // if we are only using static (un)picklers, then the Banana
    // unpickler should not know a thing about Cucumber, but duck typing
    // should work anyhow.
    val b = JSONPickle(bananaString.replace("Banana", "Cucumber")).unpickle[Banana]
    assert(b == banana)

    // the Fruit unpickler should not know anything about Cucumber, and
    // it should fail because it doesn't know to duck-type into Banana
    try {
      val f = JSONPickle(bananaString.replace("Banana", "Cucumber")).unpickle[Fruit]
      throw new Exception(s"Should have thrown on unpickle but instead parsed $f")
    } catch {
      case PicklingException(message, cause) =>
        if (!message.contains("Cucumber not recognized"))
          throw new Exception(s"Not the expected exception: $message")
    }

    // due to the annotation, the Fruit unpickler should not know
    // a thing about Apple either, even though it's a subtype of
    // Fruit.
    try {
      val f = JSONPickle(bananaString.replace("Banana", "Apple")).unpickle[Fruit]
      throw new Exception(s"Should have thrown on unpickle but instead parsed $f")
    } catch {
      case PicklingException(message, cause) =>
        if (!message.contains("Apple not recognized"))
          throw new Exception(s"Not the expected exception: $message")
    }
  }

  test("manually generate") {
    implicit val pumpkinPicklerUnpickler = PicklerUnpickler.generate[Pumpkin]
    val pumpkin = Pumpkin("Kabocha")
    val pumpkinString = pumpkin.pickle.value
    assert(JSONPickle(pumpkinString).unpickle[Pumpkin] == pumpkin)
  }
}
