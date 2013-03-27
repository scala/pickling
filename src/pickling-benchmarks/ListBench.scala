import scala.pickling._
import binary._

object ListBench extends testing.Benchmark {
  val lst = (1 to 100000).toList

  override def run() {
    val pickle = lst.pickle
    pickle.unpickle[List[Int]]
  }
}
