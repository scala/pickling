package scala.pickling.uuid.binary

import org.scalatest.FunSuite
import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._

import java.util.UUID

class UuidTest extends FunSuite {
  test("main") {
    val u = UUID.fromString("0d5d4832-af50-4a4d-837f-ef20ae862293")
    val pickle = u.pickle
    assert(pickle.toString === "BinaryPickle([0,0,0,14,106,97,118,97,46,117,116,105,108,46,85,85,73,68,13,93,72,50,-81,80,74,77,-125,127,-17,32,-82,-122,34,-109])")
    assert(pickle.unpickle[UUID] === u)
  }
}
