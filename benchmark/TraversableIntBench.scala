import scala.pickling.testing.PicklingBenchmark
import scala.util.Random

import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._

// for Java Serialization:
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream, ObjectInputStream}

object TraversableIntBench extends scala.pickling.testing.PicklingBenchmark {
  val coll = (1 to size).toVector

  override def run() {
    val pickle = coll.pickle
    val res = pickle.unpickle[Vector[Int]]
  }
}

object TraversableJavaIntBench extends scala.pickling.testing.PicklingBenchmark {
  val coll = (1 to size).toVector

  override def run() {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(coll)
    val ba = bos.toByteArray()
    // println("Bytes: " + ba.length)
    val bis = new ByteArrayInputStream(ba)
    val in = new ObjectInputStream(bis)
    val res = in.readObject.asInstanceOf[Vector[Int]]
  }
}

object TraversableKryoIntBench extends scala.pickling.testing.PicklingBenchmark {
  var ser: KryoSerializer = _
  val coll = (1 to size).toVector

  override def tearDown() {
    ser = null
  }

  override def run() {
    val rnd: Int = Random.nextInt(10)
    val arr = Array.ofDim[Byte](32 * 2048 * 2048 + rnd)
    ser = new KryoSerializer

    val pickled = ser.toBytes(coll, arr)
    val unpickled = ser.fromBytes[Vector[Int]](pickled)
  }
}