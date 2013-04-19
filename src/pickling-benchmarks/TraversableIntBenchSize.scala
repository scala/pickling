import scala.pickling._
import binary._

object TraversableIntBenchSize extends testing.Benchmark {
  override val enableOutput = false

  val size = System.getProperty("size").toInt
  val coll = (1 to size).toVector

  override def run() {
    val pickle = coll.pickle
    println(pickle.value.length)
    val res = pickle.unpickle[Vector[Int]]
  }
}
