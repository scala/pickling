package scala.pickling.graph.json

import org.scalatest.FunSuite

import scala.pickling._
import json._

// specialized to Double for now
class Vertex(val label: String, initialValue: Double = 0.0d) {
  var neighbors: List[Vertex] = List()

  var value: Double = initialValue

  var graph: Graph = null

  def connectTo(v: Vertex) {
    neighbors = v +: neighbors
  }

  override def toString = "Vertex(" + label + ")"
}

class Graph {
  var vertices: List[Vertex] = List()

  def addVertex(v: Vertex): Vertex = {
    v.graph = this
    vertices = v +: vertices
    v
  }
}

class GraphJsonTest extends FunSuite {
  test("main") {
    val g = new Graph

    // a little web graph: BBC -> MS, EPFL -> BBC, PHILIPP -> BBC, PHILIPP -> EPFL
    val d1 = g.addVertex(new Vertex("BBC"))
    val d2 = g.addVertex(new Vertex("MS"))
    val d3 = g.addVertex(new Vertex("EPFL"))
    val d4 = g.addVertex(new Vertex("PHILIPP"))
    d1.connectTo(d2)
    d3.connectTo(d1)
    d4.connectTo(d1)
    d4.connectTo(d3)

    for (v <- g.vertices) {
      v.value = 0.25d
    }

    val pickle = g.pickle
    val res = pickle.unpickle[Graph]
    assert(res.vertices.toString === "List(Vertex(PHILIPP), Vertex(EPFL), Vertex(MS), Vertex(BBC))")
  }
}
