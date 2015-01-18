import scala.pickling._
import binary._
import AllPicklers._

object VectorIntGeneratedBench extends scala.pickling.testing.PicklingBenchmark {
  val vec = (1 to size).toVector

  override def run() {
    val pickle = vec.pickle
    pickle.unpickle[Vector[Int]]
  }
}
