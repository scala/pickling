import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._

object ListIntBench extends scala.pickling.testing.PicklingBenchmark {
  val lst = (1 to size).toList

  override def run() {
    val pickle = lst.pickle
    pickle.unpickle[List[Int]]
  }
}
