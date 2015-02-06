import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._

import java.io._
import scala.util.Random

// taken from geotrellis:
trait MutableRasterData
trait IntBasedArray
final case class IntArrayRasterData(array: Array[Int], cols: Int, rows: Int)
  extends MutableRasterData with IntBasedArray

trait GeoTrellisBenchmark extends scala.pickling.testing.PicklingBenchmark {
  // println("alloc new arr of size " + size)
  val coll = (1 to size).toArray.map(_ + (16 * 1048576))
  val data = IntArrayRasterData(coll, 64, 64)
}

object GeoTrellisPicklingBench extends GeoTrellisBenchmark {
  override def run() {
    val pickle = data.pickle
    val res = pickle.unpickle[IntArrayRasterData]
  }
}

object GeoTrellisJavaBench extends GeoTrellisBenchmark {
  override def run(): Unit = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(data)
    val ba = bos.toByteArray()
    val bis = new ByteArrayInputStream(ba)
    val in = new ObjectInputStream(bis)
    val res = in.readObject.asInstanceOf[IntArrayRasterData]
  }
}

object GeoTrellisKryoBench extends GeoTrellisBenchmark {
  var ser: KryoSerializer = _

  override def tearDown() {
    ser = null
  }

  override def run() {
    val rnd: Int = Random.nextInt(10)
    //val arr = Array.ofDim[Byte](32 * 2048 * 2048 + rnd)
    val arr = Array.ofDim[Byte](32 * 2048 + rnd)
    ser = new KryoSerializer
    ser.kryo.register(data.getClass)

    val pickled = ser.toBytes(data, arr)
    // println("Size: " + pickled.length)
    val res = ser.fromBytes[IntArrayRasterData](pickled)
  }
}
