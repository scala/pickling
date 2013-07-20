import scala.pickling._
import binary._

object TraversableIntBenchSize extends scala.pickling.testing.PicklingBenchmark {
  override val enableOutput = false

  val coll = (1 to size).toVector

  override def run() {
    val pickle = coll.pickle
    println(pickle.value.length)
    val res = pickle.unpickle[Vector[Int]]
  }
}
