import scala.pickling.testing.PicklingBenchmark
import scala.util.Random

import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream, ObjectInputStream}

object TraversableIntBenchSize extends scala.pickling.testing.PicklingBenchmark {
  override val enableOutput = false

  val coll = (1 to size).toVector

  override def run() {
    val pickle = coll.pickle
    println(pickle.value.length)
  }
}

object TraversableJavaIntBenchSize extends scala.pickling.testing.PicklingBenchmark {
  override val enableOutput = false

  val coll = (1 to size).toVector

  override def run() {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(coll)
    val ba = bos.toByteArray()
    println(ba.length)
  }
}

object TraversableKryoIntBenchSize extends scala.pickling.testing.PicklingBenchmark {
  override val enableOutput = false

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
    println(pickled.length)
  }
}