import scala.collection.mutable.{Map, HashMap}
import scala.collection.mutable.ListBuffer

import scala.util.parsing.combinator._
import scala.util.parsing.input.{ Reader }
import scala.util.parsing.input.CharArrayReader.EofCh

class Vertex(val label: String) {
  var neighbors: List[Vertex] = List()

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

object GraphReader extends RegexParsers {
  override def skipWhitespace = false

  lazy val token: Parser[String] =
    """\S+""".r
  lazy val edgeline: Parser[List[String]] =
    repsep(token, whiteSpace)

  val vertices: Map[String, Vertex] = new HashMap[String, Vertex]

  def tokenize(line: String): List[String] =
    tokenize(line, x => throw new Exception(x))

  def tokenize(line: String, onError: String => Unit): List[String] =
    parse(edgeline, line.trim) match {
      case Success(args, _)     => args
      case NoSuccess(msg, rest) => onError(msg); List()
    }

  def readGraph(lines: Iterator[String], names: Map[String, String]): Graph = {
    val graph = new Graph

    for (line <- lines) {
      val labels = tokenize(line)
      //println("read labels " + labels)

      val firstLabel = labels.head.substring(0, labels.head.length - 1)
      val firstVertexOpt = vertices.get(firstLabel)
      val firstVertex =
        if (firstVertexOpt.isEmpty) graph.addVertex(new Vertex(names(firstLabel)))
        else firstVertexOpt.get
      vertices.put(firstLabel, firstVertex)

      for (targetLabel <- labels.tail) {
        val vertexOpt = vertices.get(targetLabel)
        val targetVertex = if (vertexOpt.isEmpty) {
          val newVertex = graph.addVertex(new Vertex(names(targetLabel)))
          vertices.put(targetLabel, newVertex)
          newVertex
        } else
          vertexOpt.get

        firstVertex.connectTo(targetVertex)
      }
    }
    graph
  }

  def printGraph(g: Graph) {
    for (v <- g.vertices) {
      print(v.label + ":")
      for (to <- v.neighbors) {
        print(" " + to.label)
      }
      println()
    }
  }
}

object WikiGraph extends {
    val titlesPath = "benchmark/data/titles-sorted.txt"
    val linksPath = "benchmark/data/links-sorted.txt"

    val names: Map[String, String] = new HashMap[String, String] {
      override def default(label: String) = {
        "no_title[" + label + "]"
      }
    }
    println("Building page title map...")
    val titles = scala.io.Source.fromFile(titlesPath).getLines()
    for ((title, i) <- titles zipWithIndex)
      names.put("" + i, title)

    println("Reading wikipedia graph from file... " + linksPath)
    val lines = scala.io.Source.fromFile(linksPath).getLines()
    val wikigraph = GraphReader.readGraph(lines, names)

    GraphReader.printGraph(wikigraph)
    println("#vertices: " + wikigraph.vertices.size)
}