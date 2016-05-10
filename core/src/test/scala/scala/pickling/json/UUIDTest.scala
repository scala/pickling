package pickling.run

import java.util.UUID

import org.scalatest.FunSuite

import scala.pickling.Defaults._
import scala.pickling.json._

class UUIDJsonTest extends FunSuite {
  test("main") {
    val u = UUID.fromString("0d5d4832-af50-4a4d-837f-ef20ae862293")
    val pickle = u.pickle
    assert(pickle.toString ===
      """JSONPickle({
        |  "$type": "java.util.UUID",
        |  "msb": "963005277853993549",
        |  "lsb": "-8971189009052720493"
        |})""".stripMargin)
    assert(pickle.unpickle[UUID] === u)
  }
}
