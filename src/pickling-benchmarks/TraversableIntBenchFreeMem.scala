import scala.pickling._
import binary._
import java.lang.{Runtime => JRuntime}

object TraversableIntBenchFreeMem extends testing.Benchmark {
  override val enableOutput = false

  val size = System.getProperty("size").toInt
  val coll = (1 to size).toVector
  val runtime = JRuntime.getRuntime

  override def run() {
    val pickle = coll.pickle
    val res = pickle.unpickle[Vector[Int]]

    println(runtime.freeMemory + "\t" + runtime.totalMemory)
  }
}
