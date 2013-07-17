package scala.pickling.binary.fields

import org.scalatest.FunSuite
import scala.pickling._
import binary._

class Vertex(val label: String) {

  var neighbors: List[Vertex] = List()

  var value: Double = 0.0d

  def connectTo(v: Vertex): Unit = {
    neighbors = v +: neighbors
  }

  override def toString = "Vertex(" + label + ")"
}

class BinaryFieldOrderTest extends FunSuite {
  test("main") {
    val v = new Vertex("MS")
    val pickle = v.pickle
    val res = pickle.unpickle[Vertex]
    assert(res.toString === "Vertex(MS)")
  }
}
