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
import scala.pickling.Defaults._
import scala.pickling.binary._

// for invalid characters in source files
import java.nio.charset.CodingErrorAction
import scala.io.Codec

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

  def readChunk(lines: Iterator[String], names: Map[String, String], size: Int): Graph = {
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
      if (graph.vertices.length > size) return graph
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

  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
  
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
  def readChunk(size: Int): Graph = GraphReader.readChunk(lines, names, size)

  //GraphReader.printGraph(wikigraph)
  // println("#vertices: " + wikigraph.vertices.size)
}

trait WikiGraphBenchmark extends PicklingBenchmark {
  val data = {
    // println(size)
    val result = WikiGraph.readChunk(size)
    // println("#vertices: " + result.vertices.size)
    result
  }
}

object WikiGraphPicklingBench extends WikiGraphBenchmark {
  implicit val VertexTag = FastTypeTag[Vertex]
  implicit val GraphTag = FastTypeTag[Graph]
  implicit val StringTag = FastTypeTag[String]
  implicit val ColonColonVertexTag = FastTypeTag[::[Vertex]]
  import scala.reflect.runtime.{universe => ru}
  implicit val myLittlePony: ru.Mirror = ru.runtimeMirror(getClass.getClassLoader)
  implicit val VectorVertexTag = FastTypeTag[Vector[Vertex]]
  implicit val ListVertexTag = FastTypeTag[List[Vertex]]
  implicit val NilTag = FastTypeTag[Nil.type]
  // TODO - why does this no longer compile?
  implicit val picklerNil = Pickler.generate[Nil.type]
  implicit val unpicklerNil = implicitly[Unpickler[Nil.type]]
  implicit lazy val picklerVertex: Pickler[Vertex] = {
    val picklerVertex = "boom!"
    implicitly[Pickler[Vertex]]
  }
  implicit lazy val unpicklerVertex: Unpickler[Vertex] = {
    val unpicklerVertex = "boom!"
    implicitly[Unpickler[Vertex]]
  }
  // NOTE: doesn't work well either
  // implicit object PicklerUnpicklerColonColonVertex extends scala.pickling.Pickler[::[Vertex]] with scala.pickling.Unpickler[::[Vertex]] {
  //   import scala.reflect.runtime.universe._
  //   import scala.pickling._
  //   import scala.pickling.`package`.PickleOps

  //   val format = implicitly[BinaryPickleFormat]

  //   def pickle(picklee: ::[Vertex], builder: PBuilder): Unit = {
  //     builder.hintTag(ColonColonVertexTag)
  //     builder.beginEntry(picklee)
  //     val arr = picklee.toArray
  //     val length = arr.length
  //     builder.beginCollection(arr.length)
  //     var i = 0
  //     while (i < arr.length) {
  //       builder putElement { b =>
  //         b.hintTag(VertexTag)
  //         b.hintStaticallyElidedType()
  //         arr(i).pickleInto(b)
  //       }
  //       i += 1
  //     }
  //     builder.endCollection(i)
  //     builder.endEntry()
  //   }
  //   def unpickle(tag: => scala.pickling.FastTypeTag[_], reader: PReader): Any = {
  //     val arrReader = reader.beginCollection()
  //     val length = arrReader.readLength()
  //     if (length == 1) {
  //       List(arrReader.readElement().unpickle[Vertex])
  //     } else {
  //       var buffer = scala.collection.mutable.ListBuffer[Vertex]()
  //       var i = 0
  //       while (i < length) {
  //         val r = arrReader.readElement()
  //         val elem = r.unpickle[Vertex]
  //         buffer += elem
  //         i += 1
  //       }
  //       arrReader.endCollection()
  //       buffer.toList
  //     }
  //   }
  // }
  implicit lazy val picklerUnpicklerColonColonVertex: Pickler[::[Vertex]] with Unpickler[::[Vertex]] = implicitly
  implicit lazy val picklerUnpicklerVectorVertex: Pickler[Vector[Vertex]] with Unpickler[Vector[Vertex]] = Defaults.vectorPickler[Vertex]
  implicit val picklerGraph = implicitly[Pickler[Graph]]
  implicit val unpicklerGraph = implicitly[Unpickler[Graph]]

  override def run(): Unit = {
    val pickle = data.pickle
    val res = pickle.unpickle[Graph]
  }
}

object WikiGraphJavaBench extends WikiGraphBenchmark {
  override def run(): Unit = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(data)
    val ba = bos.toByteArray()
    // println("Bytes: " + ba.length)
    val bis = new ByteArrayInputStream(ba)
    val in = new ObjectInputStream(bis)
    val res = in.readObject.asInstanceOf[Graph]
  }
}

object WikiGraphKryoBench extends WikiGraphBenchmark {
  var ser: KryoSerializer = _

  override def tearDown() {
    ser = null
  }

  override def run() {
    val rnd: Int = Random.nextInt(10)
    val arr = Array.ofDim[Byte](32 * 2048 * 2048 + rnd)
    ser = new KryoSerializer

    val pickled = ser.toBytes(data, arr)
    // println("Size: "+pickled.length)
    // TODO: uncrash this
    // val res = ser.fromBytes[Graph](pickled)
  }
}
