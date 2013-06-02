import scala.pickling._
import binary._

object TraversableIntBench extends scala.testing.PicklingBenchmark {
  val coll = (1 to size).toVector

  override def run() {
    val pickle = coll.pickle
    val res = pickle.unpickle[Vector[Int]]
  }
}
