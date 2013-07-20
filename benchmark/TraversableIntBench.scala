import scala.pickling._
import binary._

// for Java Serialization:
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream, ObjectInputStream}

object TraversableIntBench extends scala.testing.PicklingBenchmark {
  val coll = (1 to size).toVector

  override def run() {
    val pickle = coll.pickle
    val res = pickle.unpickle[Vector[Int]]
  }
}

object TraversableJavaIntBench extends scala.testing.PicklingBenchmark {
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