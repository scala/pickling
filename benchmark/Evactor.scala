import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling.binary._

import org.evactor.model.events.DataEvent
import scala.util.Random
import java.io._
import scala.reflect.runtime.{universe => ru}

object EvactorPicklingBench extends scala.pickling.testing.PicklingBenchmark {
  val time: Int = System.currentTimeMillis.toInt

  implicit lazy val myLittlePony: ru.Mirror = {
    val currentMirror = "boom!"
    ru.runtimeMirror(getClass.getClassLoader)
  }
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
  implicit lazy val picklerOfDataEvent: Pickler[DataEvent] = {
    val picklerOfDataEvent = "boom!"
    implicitly[Pickler[DataEvent]]
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
      pickles(i).unpickle[DataEvent]
      i += 1
    }
  }
}

object EvactorKryoBench extends scala.pickling.testing.PicklingBenchmark {
  var ser: KryoSerializer = _

  val time: Int = System.currentTimeMillis.toInt

  override def tearDown() {
    ser = null
  }

  override def run() {
    // random events
    val evts = for (i <- 1 to size) yield
      DataEvent("event" + i, time + Random.nextInt(100), Random.nextString(5))

    ser = new KryoSerializer
    ser.kryo.register(evts(0).getClass)

    val pickles = for (evt <- evts) yield {
      val rnd: Int = Random.nextInt(10)
      //val arr = Array.ofDim[Byte](32 * 2048 * 2048 + rnd)
      val arr = Array.ofDim[Byte](32 * 2048 + rnd)
      ser.toBytes(evt, arr)
    }

    val results = for (pickle <- pickles) yield {
      ser.fromBytes[DataEvent](pickle)
    }
  }
}

object EvactorJavaBench extends scala.pickling.testing.PicklingBenchmark {
  //val lst = (1 to size).toList

  val time: Int = System.currentTimeMillis.toInt

  override def run() = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)

        // random events
    val evts = for (i <- 1 to size) yield
      DataEvent("event" + i, time + Random.nextInt(100), Random.nextString(5))

    val pickles = for (evt <- evts) yield {
      out.writeObject(evt) // pickle evt
      bos.toByteArray()
    }

    val results = for (pickle <- pickles) yield {
      //pickle.unpickle[DataEvent]
      val bis = new ByteArrayInputStream(pickle)
      val in = new ObjectInputStream(bis)
      in.readObject.asInstanceOf[DataEvent]
    }
  }
}