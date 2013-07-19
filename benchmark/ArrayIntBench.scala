import scala.pickling._
import binary._

object ArrayIntBench extends scala.testing.PicklingBenchmark {
  val coll = (1 to size).toArray

  override def run() {
    val pickle = coll.pickle
    pickle.unpickle[Array[Int]]
  }
}
