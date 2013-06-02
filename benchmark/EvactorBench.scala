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
  implicit lazy val picklerOfDataEvent: SPickler[DataEvent] = {
    val picklerOfDataEvent = "boom!"
    implicitly[SPickler[DataEvent]]
  }
  implicit lazy val unpicklerOfDataEvent: Unpickler[DataEvent] = {
    val unpicklerOfDataEvent = "boom!"
    implicitly[Unpickler[DataEvent]]
  }

  override def run() {
    // random events
    val evts = for (i <- 1 to size) yield
      DataEvent("event" + i, time + Random.nextInt(100), Random.nextString(5))

    val pickles = for (evt <- evts) yield
      evt.pickle

    var i = 0
    while (i < size) {
      // val format: scala.pickling.binary.BinaryPickleFormat = implicitly[scala.pickling.binary.BinaryPickleFormat];
      // val reader: scala.pickling.binary.BinaryPickleReader = format.createReader(pickles(i), myLittlePony);
      // reader.hintTag(implicitly[FastTypeTag[DataEvent]](tagOfDataEvent));
      // val typeString: String = reader.beginEntryNoTag();
      // val unpickler: scala.pickling.Unpickler[_] = typeString match {
      //   case "scala.Null" => implicitly[Unpickler[Null]](Unpickler.nullPicklerUnpickler(scala.pickling.binary.`package`.pickleFormat))
      //   case "DataEvent" => implicitly[Unpickler[DataEvent]](unpicklerOfDataEvent)
      //   case _ => {
      //     val tag: FastTypeTag[_] = FastTypeTag(myLittlePony, typeString);
      //     Unpickler.genUnpickler(reader.mirror, tag)(pickleFormat)
      //   }
      // };
      // val result: Any = unpickler.unpickle(FastTypeTag(myLittlePony, typeString), reader);
      // reader.endEntry();
      // result.asInstanceOf[DataEvent]
      pickles(i).unpickle[DataEvent]
      i += 1
    }
  }
}
