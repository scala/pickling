import scala.pickling._
import binary._

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