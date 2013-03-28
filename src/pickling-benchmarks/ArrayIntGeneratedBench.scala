import scala.pickling._
import binary._

object ArrayIntGeneratedBench extends testing.Benchmark {
  val size = System.getProperty("size").toInt
  val coll = (1 to size).toArray

  override def run() {
    val pickle = coll.pickle
    pickle.unpickle[Array[Int]]
  }
}
