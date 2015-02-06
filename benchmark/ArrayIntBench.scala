import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._

object ArrayIntBench extends scala.pickling.testing.PicklingBenchmark {
  val coll = (1 to size).toArray

  override def run() {
    val pickle = coll.pickle
    pickle.unpickle[Array[Int]]
  }
}
