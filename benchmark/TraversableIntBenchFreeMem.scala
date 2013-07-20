import scala.pickling._
import binary._
import java.lang.{Runtime => JRuntime}

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