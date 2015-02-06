import scala.pickling.testing.PicklingBenchmark
import scala.util.Random

import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._
import java.lang.{Runtime => JRuntime}

// for Java Serialization:
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream, ObjectInputStream}

object TraversableIntBenchFreeMem extends scala.pickling.testing.PicklingBenchmark {
  override val enableOutput = false

  val coll = (1 to size).toVector
  val runtime = JRuntime.getRuntime

  override def run() {
    val pickle = coll.pickle
    val res = pickle.unpickle[Vector[Int]]

    println(runtime.freeMemory + "\t" + runtime.totalMemory)
  }
}

object TraversableJavaIntBenchFreeMem extends scala.pickling.testing.PicklingBenchmark {
  override val enableOutput = false

  val coll = (1 to size).toVector
  val runtime = JRuntime.getRuntime

  override def run() {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(coll)
    val ba = bos.toByteArray()
    // println("Bytes: " + ba.length)
    val bis = new ByteArrayInputStream(ba)
    val in = new ObjectInputStream(bis)
    val res = in.readObject.asInstanceOf[Vector[Int]]

    println(runtime.freeMemory + "\t" + runtime.totalMemory)
  }
}

object TraversableKryoIntBenchFreeMem extends scala.pickling.testing.PicklingBenchmark {
  override val enableOutput = false

  var ser: KryoSerializer = _
  val coll = (1 to size).toVector
  val runtime = JRuntime.getRuntime

  override def tearDown() {
    ser = null
  }

  override def run() {
    val rnd: Int = Random.nextInt(10)
    val arr = Array.ofDim[Byte](32 * 2048 * 2048 + rnd)
    ser = new KryoSerializer

    val pickled = ser.toBytes(coll, arr)
    val unpickled = ser.fromBytes[List[Int]](pickled)

    println(runtime.freeMemory + "\t" + runtime.totalMemory)
  }
}