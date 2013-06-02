import scala.pickling._
import binary._

object ArrayIntGeneratedBench extends scala.testing.PicklingBenchmark {
  val coll = (1 to size).toArray

  override def run() {
    val pickle = coll.pickle
    pickle.unpickle[Array[Int]]
  }
}
