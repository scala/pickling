package scala.pickling
package generator

import org.scalatest.FunSuite

object TopLevelObject

class SingletonGeneratorTest extends FunSuite {

  test("topLevelObject") {

    import scala.pickling.Defaults.refPicklerUnpickler
    import scala.pickling.json._
    implicit val p = PicklerUnpickler.generate[TopLevelObject.type]
    val x: TopLevelObject.type = TopLevelObject
    val pickle = scala.pickling.functions.pickle(x)
    val y: TopLevelObject.type = scala.pickling.functions.unpickle[TopLevelObject.type](pickle)
    assert(x == y)

  }
}
