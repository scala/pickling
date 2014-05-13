package scala.pickling.nested.non.primitive.subclass

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

abstract class Tree
case class Fork(left: Tree, right: Tree) extends Tree
case class Node(value: Int) extends Tree

class NestedNonPrimitiveSubclassTest extends FunSuite {
  test("main") {
    val treeStructure = Fork(Fork(Fork(Node(1), Node(2)), Node(3)), Fork(Node(4), Node(5)))
    val pckl = treeStructure.pickle
    assert(pckl.value.toString === """
      |{
      |  "$type": "scala.pickling.nested.non.primitive.subclass.Fork",
      |  "left": {
      |    "$type": "scala.pickling.nested.non.primitive.subclass.Fork",
      |    "left": {
      |      "$type": "scala.pickling.nested.non.primitive.subclass.Fork",
      |      "left": {
      |        "$type": "scala.pickling.nested.non.primitive.subclass.Node",
      |        "value": 1
      |      },
      |      "right": {
      |        "$type": "scala.pickling.nested.non.primitive.subclass.Node",
      |        "value": 2
      |      }
      |    },
      |    "right": {
      |      "$type": "scala.pickling.nested.non.primitive.subclass.Node",
      |      "value": 3
      |    }
      |  },
      |  "right": {
      |    "$type": "scala.pickling.nested.non.primitive.subclass.Fork",
      |    "left": {
      |      "$type": "scala.pickling.nested.non.primitive.subclass.Node",
      |      "value": 4
      |    },
      |    "right": {
      |      "$type": "scala.pickling.nested.non.primitive.subclass.Node",
      |      "value": 5
      |    }
      |  }
      |}
    """.stripMargin.trim)
    assert(pckl.unpickle[Tree] === treeStructure)
  }
}
