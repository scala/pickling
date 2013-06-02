import scala.pickling._
import binary._

// taken from geotrellis:
trait MutableRasterData
trait IntBasedArray
final case class IntArrayRasterData(array: Array[Int], cols: Int, rows: Int)
  extends MutableRasterData with IntBasedArray

object GeoTrellisBench extends scala.testing.PicklingBenchmark {
  val coll = (1 to size).toArray
  val data = IntArrayRasterData(coll, 64, 64)

  override def run() {
    val pickle = data.pickle
    val res = pickle.unpickle[IntArrayRasterData]
  }
}
