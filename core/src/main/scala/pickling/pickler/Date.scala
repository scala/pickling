package scala.pickling
package pickler

import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat

trait DatePicklers extends PrimitivePicklers {
  implicit val datePickler: Pickler[Date] with Unpickler[Date] =
  new Pickler[Date] with Unpickler[Date] {
    private val dateFormatTemplate = {
      val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") //use ISO_8601 format
      format.setLenient(false)
      format.setTimeZone(TimeZone.getTimeZone("UTC"))
      format
    }
    private def dateFormat = dateFormatTemplate.clone.asInstanceOf[SimpleDateFormat]

    def tag = FastTypeTag[Date]
    def pickle(picklee: Date, builder: PBuilder): Unit = {
      builder.beginEntry(picklee)

      builder.putField("value", b => {
        b.hintTag(implicitly[FastTypeTag[String]])
        b.hintStaticallyElidedType()
        stringPickler.pickle(dateFormat.format(picklee), b)
      })

      builder.endEntry()
    }
    def unpickle(tag: String, reader: PReader): Any = {
      val reader1 = reader.readField("value")
      reader1.hintTag(implicitly[FastTypeTag[String]])
      reader1.hintStaticallyElidedType()

      val tag = reader1.beginEntry()
      val result = stringPickler.unpickle(tag, reader1)
      reader1.endEntry()

      dateFormat.parse(result.asInstanceOf[String])
    }
  }
}
