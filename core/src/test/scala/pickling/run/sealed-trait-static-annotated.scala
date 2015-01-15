package scala.pickling.test.sealedtraitstaticannotated

import scala.pickling.{PickleOps, UnpickleOps, PicklingException, directSubclasses}
import scala.pickling.static._
import scala.pickling.json._
import scala.pickling.AllPicklers.{ stringPicklerUnpickler, intPicklerUnpickler, refUnpickler, nullPicklerUnpickler }

import org.scalatest.FunSuite

// we are skipping RedOrOrangeFruit here in the annotation
@directSubclasses(Array(classOf[Banana]))
trait Fruit

object Banana {
  implicit val pickler = scala.pickling.AllPicklers.genPickler[Banana]
  implicit val unpickler = scala.pickling.AllPicklers.genUnpickler[Banana]
}

// this is BEFORE the subtypes below so directKnownSubclasses probably
// won't work and this would break without the directSubclasses annotation.
object Fruit {
  implicit val pickler = scala.pickling.AllPicklers.genPickler[Fruit]
  implicit val unpickler = scala.pickling.AllPicklers.genUnpickler[Fruit]
}

sealed trait RedOrOrangeFruit extends Fruit
final case class Apple(kind: String) extends RedOrOrangeFruit
final case class Orange(ripeness: String) extends RedOrOrangeFruit
final case class Banana(something: Int) extends Fruit

final case class Cucumber(something: Int) // does not extend Fruit

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
    val appleString = try {
      (apple: Fruit).pickle.value
      assert(false) // should not be reached, we should throw
    } catch {
      case PicklingException(message, cause) =>
        assert(message.contains("Apple not recognized"))
    }

    // if we are only using static (un)picklers, then the Banana
    // unpickler should not know a thing about Cucumber.
    try {
      JSONPickle(bananaString.replace("Banana", "Cucumber")).unpickle[Banana]
    } catch {
      case PicklingException(message, cause) =>
        assert(message.contains("Cucumber not recognized"))
    }

    // due to the annotation, the Banana
    // unpickler should not know a thing about Apple.
    try {
      JSONPickle(bananaString.replace("Banana", "Apple")).unpickle[Banana]
    } catch {
      case PicklingException(message, cause) =>
        assert(message.contains("Apple not recognized"))
    }
  }
}
