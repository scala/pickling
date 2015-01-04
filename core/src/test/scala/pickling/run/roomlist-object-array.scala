package scala.pickling.test.roomlist.objectarray

import org.scalatest.FunSuite
import scala.pickling._
import json._
import AllPicklers._

case class Room(val name: String)
case class RoomList(val rooms: Array[Room])

class RoomListObjectArrayTest extends FunSuite {
  test("main") {
    val rl = RoomList(Array(Room("foo"), Room("biz"), Room("bang")))
    val p = rl.pickle
    //println(p.toString)
    val jsn = """JSONPickle({
      |  "tpe": "scala.pickling.test.roomlist.objectarray.RoomList",
      |  "rooms": {
      |    "elems": [
      |      {
      |      "tpe": "scala.pickling.test.roomlist.objectarray.Room",
      |      "name": "foo"
      |    },
      |      {
      |      "tpe": "scala.pickling.test.roomlist.objectarray.Room",
      |      "name": "biz"
      |    },
      |      {
      |      "tpe": "scala.pickling.test.roomlist.objectarray.Room",
      |      "name": "bang"
      |    }
      |    ]
      |  }
      |})""".stripMargin.trim

    assert(p.toString === jsn)
    assert(rl.pickle.unpickle[RoomList].rooms.sameElements(rl.rooms))
  }
}
