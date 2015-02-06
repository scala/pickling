import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._

object VectorIntGeneratedBench extends scala.pickling.testing.PicklingBenchmark {
  val vec = (1 to size).toVector

  override def run() {
    val pickle = vec.pickle
    pickle.unpickle[Vector[Int]]
  }
}
