import scala.pickling._
import binary._

object ListIntGeneratedBench extends testing.Benchmark {
  val size = System.getProperty("size").toInt
  val lst = (1 to size).toList

  override def run() {
    val pickle = lst.pickle
    pickle.unpickle[List[Int]]
  }
}
