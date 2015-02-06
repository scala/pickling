import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._
import java.io._
import scala.util.Random

// taken from SparkLR:
package spark.util { final class Vector(val elements: Array[Double]) extends Serializable { override def toString = s"""Vector(${elements.mkString(", ")})""" } }
import scala.collection.mutable.ArrayBuffer
import spark.util.Vector
final case class DataPoint(x: Vector, y: Double) extends Serializable

trait SparkLRBenchmark extends scala.pickling.testing.PicklingBenchmark {
  val data = {
    def generatePoint(i: Int) = {
      val y = if (i % 2 == 0) -1 else 1
      val x = {
        val elements = new Array[Double](10)
        for (i <- 0 until 10) elements(i) = y * 0.7
        new Vector(elements)
      }
      DataPoint(x, y)
    }
    val buffer = ArrayBuffer[DataPoint]()
    for (i <- 1 to size) buffer += generatePoint(i)
    buffer
  }
}

object SparkLRPicklingBench extends SparkLRBenchmark {
  override def run() {
    val pickle = data.pickle
    val res = pickle.unpickle[ArrayBuffer[DataPoint]]
  }
}

object SparkLRJavaBench extends SparkLRBenchmark {
  override def run(): Unit = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(data)
    val ba = bos.toByteArray()
    val bis = new ByteArrayInputStream(ba)
    val in = new ObjectInputStream(bis)
    val res = in.readObject.asInstanceOf[ArrayBuffer[DataPoint]]
  }
}

object SparkLRKryoBench extends SparkLRBenchmark {
  var ser: KryoSerializer = _

  override def tearDown() {
    ser = null
  }

  override def run() {
    val rnd: Int = Random.nextInt(10)
    val arr = Array.ofDim[Byte](32 * 2048 * 2048 + rnd)
    ser = new KryoSerializer

    val pickled = ser.toBytes(data, arr)
    val res = ser.fromBytes[ArrayBuffer[DataPoint]](pickled)
  }
}
