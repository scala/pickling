import scala.pickling._
import binary._

import org.evactor.model.events.DataEvent
import scala.util.Random

object EvactorBench extends scala.testing.PicklingBenchmark {
  val time: Int = System.currentTimeMillis.toInt

  implicit lazy val tagOfDataEvent: FastTypeTag[DataEvent] = {
    val tagOfDataEvent = "boom!"
    implicitly[FastTypeTag[DataEvent]]
  }
  implicit lazy val tagOfNull: FastTypeTag[Null] = {
    val tagOfNull = "boom!"
    implicitly[FastTypeTag[Null]]
  }
  implicit lazy val tagOfString: FastTypeTag[String] = {
    val tagOfString = "boom!"
    implicitly[FastTypeTag[String]]
  }
  implicit lazy val tagOfInt: FastTypeTag[Int] = {
    val tagOfInt = "boom!"
    implicitly[FastTypeTag[Int]]
  }

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
