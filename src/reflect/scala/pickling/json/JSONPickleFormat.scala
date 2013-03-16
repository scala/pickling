package scala.pickling

package object json {
  implicit val pickleFormat = new JSONPickleFormat
  implicit def toJSONPickle(value: String): JSONPickle = new JSONPickle(value)
}

package json {
  import language.experimental.macros

  import scala.reflect.api.Universe
  import scala.reflect.runtime.{universe => ru}
  import scala.reflect.macros.Macro
  import scala.util.parsing.json._

  case class JSONPickle(val value: String) extends Pickle {
    type ValueType = String
    type PickleFormatType = JSONPickleFormat
  }

  class JSONPickleFormat extends PickleFormat {
    import ir._
    import ru.definitions._

    type PickleType = JSONPickle
    override def instantiate = macro JSONPickleInstantiate.impl

    // def formatRT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, picklee: Any, fields: irs.FieldIR => Pickle): JSONPickle = {
    def formatRT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, picklee: Any, fields: irs.FieldIR => Pickle): Pickle = {
      def objectPrefix(tpe: irs.uni.Type) = "{\n  \"tpe\": \"" + tpe.typeSymbol.name.toString + "\",\n"
      val objectSuffix = "\n}"
      val fieldSeparator = ",\n"
      def fieldPrefix(fir: irs.FieldIR) = "  \"" + fir.name + "\": "
      val fieldSuffix = ""
      JSONPickle {
        objectPrefix(cir.tpe) + {
          val formattedFields = cir.fields.map(fld => fieldPrefix(fld) + fields(fld).value)
          formattedFields mkString fieldSeparator
        } + objectSuffix
      }
    }

    //TODO: seems unneeded after move to parseTpe, parseField, etc.
    def unpickleTpe(stpe: String, mirror: ru.Mirror): ru.Type = {
      // TODO: support polymorphic types as serialized above in formatCT/formatRT
      mirror.staticClass(stpe).asType.toType
    }

    def getObject(p: PickleType): Any = JSON.parseRaw(p.value).get

    def getType(obj: Any, mirror: ru.Mirror): ru.Type = {
      val JSONObject(data) = obj
      unpickleTpe(data("tpe").toString, mirror)
    }

    def getField(obj: Any, name: String): Any = obj match {
      case JSONObject(data) => data(name)
    }

    def getPrimitive(obj: Any, tpe: ru.Type, name: String): Any = obj match {
      case JSONObject(data) =>
        tpe match {
          case tp if tp =:= IntClass.toType    => data(name).asInstanceOf[Double].toInt
          case tp if tp =:= StringClass.toType => data(name).toString
        }
    }

    /** Returns partial pickle */
    def putType(tpe: ru.Type): PickleType =
      JSONPickle("{\n  \"tpe\": \"" + tpe.typeSymbol.name.toString + "\"")

    /** Adds field to `partial` pickle, using `state` to guide the pickling */
    def putField(partial: PickleType, state: Any, name: String, value: Any): PickleType =
      JSONPickle(partial.value + ",\n" + "  \"" + name + "\": " + value)

    /** Adds field of primitive type to `partial` pickle */
    def putPrimitive(partial: PickleType, state: Any, tpe: ru.Type, name: String, value: AnyVal): PickleType = {
      val valueToWrite = formatPrimitive(tpe, value).value
      JSONPickle(partial.value + ",\n" + "  \"" + name + "\": " + valueToWrite)
    }

    def putObjectSuffix(partial: PickleType, state: Any): PickleType =
      JSONPickle(partial.value + "\n}")

    def formatPrimitive(tpe: ru.Type, value: Any): PickleType =
      if (tpe =:= CharClass.toType || tpe =:= StringClass.toType)
        JSONPickle("\"" + JSONFormat.quoteString(value.toString) + "\"")
      else
        JSONPickle(value.toString)
  }

  trait JSONPickleInstantiate extends Macro {
    def impl = c.universe.EmptyTree updateAttachment pickleFormat
  }
}
