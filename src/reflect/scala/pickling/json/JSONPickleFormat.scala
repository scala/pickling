package scala.pickling

package object json {
  implicit val pickleFormat: JSONPickleFormat = new JSONPickleFormat
  implicit def toJSONPickle(value: String): JSONPickle = JSONPickle(value)
}

package json {
  import scala.reflect.runtime.universe._
  import definitions._
  import scala.util.parsing.json._
  import scala.collection.mutable.{StringBuilder, Stack}
  import scala.reflect.synthetic._

  case class JSONPickle(value: String) extends Pickle {
    type ValueType = String
    type PickleFormatType = JSONPickleFormat
  }

  class JSONPickleFormat extends PickleFormat {
    type PickleType = JSONPickle
    def createBuilder() = new JSONPickleBuilder(this)
    def createReader(pickle: JSONPickle, mirror: Mirror) = {
      JSON.parseRaw(pickle.value) match {
        case Some(raw) => new JSONPickleReader(raw, mirror, this)
        case None => throw new PicklingException("failed to parse \"" + pickle.value + "\" as JSON")
      }
    }
    def isPrimitive(tpe: Type): Boolean = {
      val sym = tpe.typeSymbol.asClass
      sym == NullClass || sym.isPrimitive || sym == StringClass
    }
  }

  class JSONPickleBuilder(format: JSONPickleFormat) extends PickleBuilder with PickleTools {
    private val buf = new StringBuilder()
    private val tags = new Stack[TypeTag[_]]()
    private val primitives = Map[TypeTag[_], Any => Unit](
      ReifiedNull.tag -> ((picklee: Any) => buf ++= "null"),
      ReifiedInt.tag -> ((picklee: Any) => buf ++= picklee.toString),
      ReifiedBoolean.tag -> ((picklee: Any) => buf ++= picklee.toString),
      ReifiedString.tag -> ((picklee: Any) => buf ++= "\"" + JSONFormat.quoteString(picklee.toString) + "\"")
    )
    def beginEntry(picklee: Any): this.type = withHints { hints =>
      tags.push(hints.tag)
      if (primitives.contains(hints.tag)) {
        assert(hints.isElidedType)
        primitives(hints.tag)(picklee)
      } else {
        buf ++= "{\n"
        if (!hints.isElidedType) buf ++= "  \"tpe\": \"" + typeToString(hints.tag.tpe) + "\""
      }
      this
    }
    def putField(name: String, pickler: this.type => Unit): this.type = {
      assert(!primitives.contains(tags.top), tags.top)
      if (buf.toString.trim.last != '{') buf ++= ",\n" // TODO: very inefficient, but here we don't care much about performance
      buf ++= "  \"" + name + "\": "
      pickler(this)
      this
    }
    def endEntry(): Unit = {
      if (primitives.contains(tags.pop())) () // do nothing
      else buf ++= "\n}"
    }
    def beginCollection(length: Int): this.type = {
      putField("elems", b => ())
      buf ++= "["
      this
    }
    def putElement(pickler: this.type => Unit): this.type = {
      if (buf.toString.trim.last != '[') buf ++= ", " // TODO: very inefficient, but here we don't care much about performance
      pickler(this)
      this
    }
    def endCollection(): Unit = buf ++= "]"
    def result(): JSONPickle = {
      assert(tags.isEmpty, tags)
      JSONPickle(buf.toString)
    }
  }

  class JSONPickleReader(datum: Any, val mirror: Mirror, format: JSONPickleFormat) extends PickleReader with PickleTools {
    private var lastReadTag: TypeTag[_] = null
    private val primitives = Map[TypeTag[_], () => Any](
      ReifiedNull.tag -> (() => null),
      ReifiedInt.tag -> (() => datum.asInstanceOf[Double].toInt),
      ReifiedBoolean.tag -> (() => datum.asInstanceOf[Boolean]),
      ReifiedString.tag -> (() => datum.asInstanceOf[String])
    )
    def beginEntry(): TypeTag[_] = withHints { hints =>
      lastReadTag = {
        if (datum == null) ReifiedNull.tag
        else if (hints.isElidedType) hints.tag
        else {
          datum match {
            case JSONObject(fields) if fields.contains("tpe") => TypeTag(typeFromString(mirror, fields("tpe").asInstanceOf[String]))
            case JSONObject(fields) => hints.tag
          }
        }
      }
      lastReadTag
    }
    def atPrimitive: Boolean = !atObject
    def readPrimitive(): Any = primitives(lastReadTag)()
    def atObject: Boolean = datum.isInstanceOf[JSONObject]
    def readField(name: String): JSONPickleReader = {
      datum match {
        case JSONObject(fields) => new JSONPickleReader(fields(name), mirror, format)
      }
    }
    def endEntry(): Unit = {}
    def beginCollection(): PickleReader = readField("elems")
    def readLength(): Int = {
      datum match {
        case JSONArray(list) => list.length
      }
    }
    private var i = 0
    def readElement(): PickleReader = {
      val reader = {
        datum match {
          case JSONArray(list) => new JSONPickleReader(list(i), mirror, format)
        }
      }
      i += 1
      reader
    }
    def endCollection(): Unit = {}
  }
}
