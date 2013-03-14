import scala.pickling._
import json._

abstract class Tree
case class Fork(left: Tree, right: Tree) extends Tree
case class Node(value: Int) extends Tree

object Test extends App {
  val treeStructure = Fork(Fork(Fork(Node(1), Node(2)), Node(3)), Fork(Node(4), Node(5)))
  val pckl = treeStructure.pickle
  println(pckl.value)
  println(pckl.unpickle[Tree])
}
