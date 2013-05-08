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
    private var nindent = 0
    private def indent() = nindent += 1
    private def unindent() = nindent -= 1
    private var pendingIndent = false
    private def append(s: String) = {
      val sindent = if (pendingIndent) "  " * nindent else ""
      buf ++= (sindent + s)
      pendingIndent = false
    }
    private def appendLine(s: String = "") = {
      append(s + "\n")
      pendingIndent = true
    }
    private val tags = new Stack[FastTypeTag[_]]()
    private val primitives = Map[String, Any => Unit](
      fastTypeTag[Null].key -> ((picklee: Any) => append("null")),
      fastTypeTag[Int].key -> ((picklee: Any) => append(picklee.toString)),
      fastTypeTag[Boolean].key -> ((picklee: Any) => append(picklee.toString)),
      fastTypeTag[String].key -> ((picklee: Any) => append("\"" + JSONFormat.quoteString(picklee.toString) + "\"")),
      fastTypeTag[java.lang.String].key -> ((picklee: Any) => append("\"" + JSONFormat.quoteString(picklee.toString) + "\""))
    )
    def beginEntry(picklee: Any): this.type = withHints { hints =>
      indent()
      tags.push(hints.tag)
      if (primitives.contains(hints.tag.key)) {
        if (hints.isElidedType) primitives(hints.tag.key)(picklee)
        else {
          appendLine("{")
          appendLine("\"tpe\": \"" + typeToString(hints.tag.tpe) + "\",")
          append("\"value\": ")
          primitives(hints.tag.key)(picklee)
          appendLine("")
          unindent()
          append("}")
          indent()
        }
      } else {
        appendLine("{")
        if (!hints.isElidedType) append("\"tpe\": \"" + typeToString(hints.tag.tpe) + "\"")
      }
      this
    }
    def putField(name: String, pickler: this.type => Unit): this.type = {
      assert(!primitives.contains(tags.top.key), tags.top)
      if (buf.toString.trim.last != '{') appendLine(",") // TODO: very inefficient, but here we don't care much about performance
      append("\"" + name + "\": ")
      pickler(this)
      this
    }
    def endEntry(): Unit = {
      unindent()
      if (primitives.contains(tags.pop().key)) () // do nothing
      else { appendLine(); append("}") }
    }
    def beginCollection(length: Int): this.type = {
      putField("elems", b => ())
      appendLine("[")
      // indent()
      this
    }
    def putElement(pickler: this.type => Unit): this.type = {
      if (buf.toString.trim.last != '[') appendLine(",") // TODO: very inefficient, but here we don't care much about performance
      pickler(this)
      this
    }
    def endCollection(l: Int): Unit = {
      appendLine()
      append("]")
      // unindent()
    }
    def result(): JSONPickle = {
      assert(tags.isEmpty, tags)
      JSONPickle(buf.toString)
    }
  }

  class JSONPickleReader(datum: Any, val mirror: Mirror, format: JSONPickleFormat) extends PickleReader with PickleTools {
    private var lastReadTag: FastTypeTag[_] = null
    private val primitives = Map[String, () => Any](
      fastTypeTag[Null].key -> (() => null),
      fastTypeTag[Int].key -> (() => datum.asInstanceOf[Double].toInt),
      fastTypeTag[Boolean].key -> (() => datum.asInstanceOf[Boolean]),
      fastTypeTag[String].key -> (() => datum.asInstanceOf[String]),
      fastTypeTag[java.lang.String].key -> (() => datum.asInstanceOf[String])
    )
    private def mkNestedReader(datum: Any) = {
      val nested = new JSONPickleReader(datum, mirror, format)
      if (this.areHintsPinned) {
        nested.areHintsPinned = true
        nested.hints = hints
        nested.lastReadTag = lastReadTag
      }
      nested
    }
    def beginEntryNoTag(): String = beginEntry().key
    def beginEntry(): FastTypeTag[_] = withHints { hints =>
      lastReadTag = {
        if (datum == null) fastTypeTag[Null]
        else if (hints.isElidedType) hints.tag
        else {
          datum match {
            case JSONObject(fields) if fields.contains("tpe") => FastTypeTag(mirror, typeFromString(mirror, fields("tpe").asInstanceOf[String]), fields("tpe").asInstanceOf[String])
            case JSONObject(fields) => hints.tag
          }
        }
      }
      lastReadTag
    }
    def atPrimitive: Boolean = primitives.contains(lastReadTag.key)
    def readPrimitive(): Any = {
      datum match {
        case JSONObject(fields) => mkNestedReader(fields("value")).primitives(lastReadTag.key)()
        case _ => primitives(lastReadTag.key)()
      }
    }
    def readArray(): Any = ??? //TODO
    def atObject: Boolean = datum.isInstanceOf[JSONObject]
    def readField(name: String): JSONPickleReader = {
      datum match {
        case JSONObject(fields) => mkNestedReader(fields(name))
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
          case JSONArray(list) => mkNestedReader(list(i))
        }
      }
      i += 1
      reader
    }
    def endCollection(): Unit = {}
  }
}
