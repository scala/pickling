import scala.pickling._
import binary._

import org.evactor.model.events.DataEvent
import scala.util.Random

object EvactorBench extends testing.Benchmark {
  val size = System.getProperty("size").toInt
  //val coll = (1 to size).toArray

  val time: Int = System.currentTimeMillis.toInt

  override def run() {
    // random events
    val evts = for (i <- 1 to size) yield
      DataEvent("event" + i, time + Random.nextInt(100), Random.nextString(5))

    val pickles = for (evt <- evts) yield
      evt.pickle
    
    val results = for (pickle <- pickles) yield
      pickle.unpickle[DataEvent]
  }
}
