package scala.pickling.privatepublicctorstest

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._

trait Command

object Commands {
   case class SomeCommand(x: String) extends Command
}

case class CommandMessage(cmd: Command)
case class CommandMessage2(cmd: AnyRef)

class NestedObjectTest extends FunSuite {
  test("json") {
    import json._
    val cmd = Commands.SomeCommand("hey, this is a command!")
    val c = CommandMessage(cmd)
    val p: JSONPickle = c.pickle
    val up = p.unpickle[CommandMessage]
    assert(c.cmd == up.cmd)
  }

  test("binary") {
    import binary._
    val cmd = Commands.SomeCommand("hey, this is a command!")
    val c = CommandMessage(cmd)
    val p: BinaryPickle = c.pickle
    val up = p.unpickle[CommandMessage]
    assert(c.cmd == up.cmd)
  }
}

class NestedObjectRuntimeTest extends FunSuite {
  test("json") {
    import json._
    val cmd = Commands.SomeCommand("hey, this is a command!")
    val c = CommandMessage2(cmd)
    val p: JSONPickle = c.pickle
    val up = p.unpickle[CommandMessage2]
    assert(c.cmd == up.cmd)
  }

  test("binary") {
    import binary._
    val cmd = Commands.SomeCommand("hey, this is a command!")
    val c = CommandMessage2(cmd)
    val p: BinaryPickle = c.pickle
    val up = p.unpickle[CommandMessage2]
    assert(c.cmd == up.cmd)
  }
}
