package scala.pickling.test.sealedtraitstatic

import scala.pickling.PicklingException
import scala.pickling.ops._
import scala.pickling.static._
import scala.pickling.json._
import scala.pickling.allPicklers._

import org.scalatest.FunSuite


sealed trait Fruit

sealed trait RedOrOrangeFruit extends Fruit
final case class Apple(kind: String) extends RedOrOrangeFruit
final case class Orange(ripeness: String) extends RedOrOrangeFruit
final case class Banana(something: Int) extends Fruit

final case class Cucumber(something: Int) // does not extend Fruit

object Fruit {
  implicit val pickler = scala.pickling.allPicklers.genPickler[Fruit]
  implicit val unpickler = scala.pickling.allPicklers.genUnpickler[Fruit]
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

    // if we are only using static (un)picklers, then the Banana
    // unpickler should not know a thing about Cucumber.
    try {
      JSONPickle(bananaString.replace("Banana", "Cucumber")).unpickle[Banana]
    } catch {
      case PicklingException(message, cause) =>
        assert(message.contains("Cucumber not recognized"))
    }
  }
}
