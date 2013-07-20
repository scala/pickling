import java.lang.{Runtime => JRuntime}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream, ObjectInputStream}

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