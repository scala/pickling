import scala.pickling._
import binary._

object VectorIntGeneratedBench extends testing.Benchmark {
  val size = System.getProperty("size").toInt
  val vec = (1 to size).toVector

  override def run() {
    val pickle = vec.pickle
    pickle.unpickle[Vector[Int]]
  }
}
