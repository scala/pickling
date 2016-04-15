package scala.pickling
package pickler

import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat

trait DatePicklers extends PrimitivePicklers {
  implicit val datePickler: Pickler[Date] with Unpickler[Date] =
  new AbstractPicklerUnpickler[Date] with AutoRegister[Date] {
    private val dateFormatTemplate = {
      val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") //use ISO_8601 format
      format.setLenient(false)
      format.setTimeZone(TimeZone.getTimeZone("UTC"))
      format
    }
    private def dateFormat = dateFormatTemplate.clone.asInstanceOf[SimpleDateFormat]

    lazy val tag = FastTypeTag[Date]("java.util.Date")
    def pickle(picklee: Date, builder: PBuilder): Unit = {
      builder.beginEntry(picklee, tag)

      builder.putField("value", b => {
        b.hintElidedType(implicitly[FastTypeTag[String]])
        stringPickler.pickle(dateFormat.format(picklee), b)
      })

      builder.endEntry()
    }
    def unpickle(tag: String, reader: PReader): Any = {
      val reader1 = reader.readField("value")
      reader1.hintElidedType(implicitly[FastTypeTag[String]])
      val result = stringPickler.unpickleEntry(reader1)
      dateFormat.parse(result.asInstanceOf[String])
    }
  }
  internal.currentRuntime.picklers.registerPicklerUnpickler(datePickler)
}
