import scala.util.parsing.combinator._
import scala.util.parsing.input.Reader
import scala.util.parsing.input.CharArrayReader.EofCh

import scala.collection.mutable.{Map, HashMap}
import scala.pickling.testing.PicklingBenchmark
import scala.io.Source
import scala.util.Random

// for Java Serialization:
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream, ObjectInputStream}

import scala.pickling._
import binary._

final class Vertex(val label: String, var neighbors: List[Vertex]) extends Serializable {

  //var graph: Graph = null

  def connectTo(v: Vertex) {
    neighbors = v +: neighbors
  }

  def sameAs(other: Vertex): Boolean = {
    (this ne other) &&
    this.label == other.label && (
      this.neighbors.length == other.neighbors.length &&
      this.neighbors.zip(other.neighbors).forall {
        case (thisv, otherv) => thisv.label == otherv.label
      }
    )
  }

  override def toString = "Vertex(" + label + ")"
}

final class Graph extends Serializable {
  var vertices: Vector[Vertex] = Vector()

  def addVertex(v: Vertex): Vertex = {
    //v.graph = this
    vertices = v +: vertices
    v
  }

  def sameAs(other: Graph): Boolean = {
    (this ne other) &&
    this.vertices.length == other.vertices.length &&
    this.vertices.zip(other.vertices).forall {
      case (thisv, otherv) => thisv.sameAs(otherv)
    }
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
        if (firstVertexOpt.isEmpty) graph.addVertex(new Vertex(names(firstLabel), List()))
        else firstVertexOpt.get
      vertices.put(firstLabel, firstVertex)

      val targetVertices = for (targetLabel <- labels.tail) yield {
        val vertexOpt = vertices.get(targetLabel)

        if (vertexOpt.isEmpty) {
          val newVertex = graph.addVertex(new Vertex(names(targetLabel), List()))
          vertices.put(targetLabel, newVertex)
          newVertex
        } else {
          vertexOpt.get
        }
      }

      firstVertex.neighbors = targetVertices
    }

    graph
  }

  def printGraph(g: Graph): Unit = {
    for (v <- g.vertices) {
      print(v.label + ":")
      for (to <- v.neighbors) {
        print(" " + to.label)
      }
      println()
    }
  }
}

object WikiGraph {
  val titlesPath = "benchmark/data/titles-sorted.txt"
  val linksPath  = "benchmark/data/links-sorted.txt"

  val names: Map[String, String] = new HashMap[String, String] {
    override def default(label: String) = {
      "no_title[" + label + "]"
    }
  }
  // println("Building page title map...")
  val titles = Source.fromFile(titlesPath).getLines()
  for ((title, i) <- titles.zipWithIndex)
    names.put("" + i, title)

  // println("Reading wikipedia graph from file... " + linksPath)
  val lines: Iterator[String] = Source.fromFile(linksPath).getLines()
  val wikigraph: Graph        = GraphReader.readGraph(lines, names)

  //GraphReader.printGraph(wikigraph)
  // println("#vertices: " + wikigraph.vertices.size)
}

object WikiGraphPicklingSlowBench extends PicklingBenchmark {
  override def run(): Unit = {
    val pickle = WikiGraph.wikigraph.pickle
    scala.pickling.`package`.clearPicklees()
    val res = pickle.unpickle[Graph]
    scala.pickling.`package`.clearUnpicklees()
  }
}

object WikiGraphPicklingFastBench extends PicklingBenchmark {
  override def run(): Unit = {
    val pickle = WikiGraph.wikigraph.pickle
    val res = pickle.unpickle[Graph]
  }
}

object WikiGraphJavaBench extends PicklingBenchmark {
  override def run(): Unit = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(WikiGraph.wikigraph)
    val ba = bos.toByteArray()
    // println("Bytes: " + ba.length)
    // TODO: uncrash this
    // val bis = new ByteArrayInputStream(ba)
    // val in = new ObjectInputStream(bis)
    // val res = in.readObject.asInstanceOf[Graph]
  }
}

object WikiGraphKryoBench extends scala.pickling.testing.Benchmark {
  var ser: KryoSerializer = _

  override def tearDown() {
    ser = null
  }

  override def run() {
    val rnd: Int = Random.nextInt(10)
    val arr = Array.ofDim[Byte](32 * 2048 * 2048 + rnd)
    ser = new KryoSerializer

    val pickled = ser.toBytes(WikiGraph.wikigraph, arr)
    // println("Size: "+pickled.length)
    val res = ser.fromBytes[Graph](pickled)
  }
}
