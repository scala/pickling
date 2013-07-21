package scala.pickling

import scala.language.implicitConversions

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
    type OutputType = Output[String]
    def createBuilder() = new JSONPickleBuilder(this, new StringOutput)
    def createBuilder(out: Output[String]): PBuilder = new JSONPickleBuilder(this, out)
    def createReader(pickle: JSONPickle, mirror: Mirror) = {
      JSON.parseRaw(pickle.value) match {
        case Some(raw) => new JSONPickleReader(raw, mirror, this)
        case None => throw new PicklingException("failed to parse \"" + pickle.value + "\" as JSON")
      }
    }
  }

  class JSONPickleBuilder(format: JSONPickleFormat, buf: Output[String]) extends PBuilder with PickleTools {
    // private val buf = new StringBuilder()
    private var nindent = 0
    private def indent() = nindent += 1
    private def unindent() = nindent -= 1
    private var pendingIndent = false
    private var lastIsBrace = false
    private var lastIsBracket = false
    @inline def hintKnownSize(knownSize: Int): Unit = { /* do nothing */ }
    private def append(s: String) = {
      val sindent = if (pendingIndent) "  " * nindent else ""
      buf.put(sindent + s)
      pendingIndent = false
      val trimmed = s.trim
      if (trimmed.nonEmpty) {
        val lastChar = trimmed.last
        lastIsBrace = lastChar == '{'
        lastIsBracket = lastChar == '['
      }
    }
    private def appendLine(s: String = "") = {
      append(s + "\n")
      pendingIndent = true
    }
    private val tags = new Stack[FastTypeTag[_]]()
    private def pickleArray(arr: Array[_], tag: FastTypeTag[_]) = {
      unindent()
      appendLine("[")
      hintStaticallyElidedType()
      hintTag(tag)
      pinHints()
      var i = 0
      while (i < arr.length) {
        putElement(b => b.beginEntry(arr(i)).endEntry())
        i += 1
      }
      unpinHints()
      appendLine("")
      append("]")
      indent()
    }
    private val primitives = Map[String, Any => Unit](
      FastTypeTag.Null.key -> ((picklee: Any) => append("null")),
      FastTypeTag.Ref.key -> ((picklee: Any) => throw new Error("fatal error: shouldn't be invoked explicitly")),
      FastTypeTag.Int.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Long.key -> ((picklee: Any) => append("\"" + JSONFormat.quoteString(picklee.toString) + "\"")),
      FastTypeTag.Short.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Double.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Float.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Boolean.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Byte.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Char.key -> ((picklee: Any) => append("\"" + JSONFormat.quoteString(picklee.toString) + "\"")),
      FastTypeTag.ScalaString.key -> ((picklee: Any) => append("\"" + JSONFormat.quoteString(picklee.toString) + "\"")),
      FastTypeTag.JavaString.key -> ((picklee: Any) => append("\"" + JSONFormat.quoteString(picklee.toString) + "\"")),
      FastTypeTag.ArrayByte.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Byte]], FastTypeTag.Byte)),
      FastTypeTag.ArrayShort.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Short]], FastTypeTag.Short)),
      FastTypeTag.ArrayChar.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Char]], FastTypeTag.Char)),
      FastTypeTag.ArrayInt.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Int]], FastTypeTag.Int)),
      FastTypeTag.ArrayLong.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Long]], FastTypeTag.Long)),
      FastTypeTag.ArrayBoolean.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Boolean]], FastTypeTag.Boolean)),
      FastTypeTag.ArrayFloat.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Float]], FastTypeTag.Float)),
      FastTypeTag.ArrayDouble.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Double]], FastTypeTag.Double))
    )
    def beginEntry(picklee: Any): this.type = withHints { hints =>
      indent()
      if (hints.oid != -1) {
        tags.push(FastTypeTag.Ref)
        append("{ \"$ref\": " + hints.oid + " }")
      } else {
        tags.push(hints.tag)
        if (primitives.contains(hints.tag.key)) {
          if (hints.isElidedType) primitives(hints.tag.key)(picklee)
          else {
            appendLine("{")
            appendLine("\"tpe\": \"" + typeToString(hints.tag.tpe) + "\",")
            append("\"value\": ")
            indent()
            primitives(hints.tag.key)(picklee)
            unindent()
            appendLine("")
            unindent()
            append("}")
            indent()
          }
        } else {
          appendLine("{")
          if (!hints.isElidedType) append("\"tpe\": \"" + typeToString(hints.tag.tpe) + "\"")
        }
      }
      this
    }
    def putField(name: String, pickler: this.type => Unit): this.type = {
      // assert(!primitives.contains(tags.top.key), tags.top)
      if (!lastIsBrace) appendLine(",") // TODO: very inefficient, but here we don't care much about performance
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
      if (!lastIsBracket) appendLine(",") // TODO: very inefficient, but here we don't care much about performance
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

  class JSONPickleReader(var datum: Any, val mirror: Mirror, format: JSONPickleFormat) extends PReader with PickleTools {
    private var lastReadTag: FastTypeTag[_] = null
    private val primitives = Map[String, () => Any](
      FastTypeTag.Null.key -> (() => null),
      FastTypeTag.Ref.key -> (() => lookupUnpicklee(datum.asInstanceOf[JSONObject].obj("$ref").asInstanceOf[Double].toInt)),
      FastTypeTag.Int.key -> (() => datum.asInstanceOf[Double].toInt),
      FastTypeTag.Short.key -> (() => datum.asInstanceOf[Double].toShort),
      FastTypeTag.Double.key -> (() => datum.asInstanceOf[Double]),
      FastTypeTag.Float.key -> (() => datum.asInstanceOf[Double].toFloat),
      FastTypeTag.Long.key -> (() => datum.asInstanceOf[String].toLong),
      FastTypeTag.Byte.key -> (() => datum.asInstanceOf[Double].toByte),
      FastTypeTag.Boolean.key -> (() => datum.asInstanceOf[Boolean]),
      FastTypeTag.Char.key -> (() => datum.asInstanceOf[String].head),
      FastTypeTag.ScalaString.key -> (() => datum.asInstanceOf[String]),
      FastTypeTag.JavaString.key -> (() => datum.asInstanceOf[String]),
      FastTypeTag.ArrayByte.key -> (() => datum.asInstanceOf[JSONArray].list.map(el => el.asInstanceOf[Double].toByte).toArray),
      FastTypeTag.ArrayShort.key -> (() => datum.asInstanceOf[JSONArray].list.map(el => el.asInstanceOf[Double].toShort).toArray),
      FastTypeTag.ArrayChar.key -> (() => datum.asInstanceOf[JSONArray].list.map(el => el.asInstanceOf[String].head).toArray),
      FastTypeTag.ArrayInt.key -> (() => datum.asInstanceOf[JSONArray].list.map(el => el.asInstanceOf[Double].toInt).toArray),
      FastTypeTag.ArrayLong.key -> (() => datum.asInstanceOf[JSONArray].list.map(el => el.asInstanceOf[String].toLong).toArray),
      FastTypeTag.ArrayBoolean.key -> (() => datum.asInstanceOf[JSONArray].list.map(el => el.asInstanceOf[Boolean]).toArray),
      FastTypeTag.ArrayFloat.key -> (() => datum.asInstanceOf[JSONArray].list.map(el => el.asInstanceOf[Double].toFloat).toArray),
      FastTypeTag.ArrayDouble.key -> (() => datum.asInstanceOf[JSONArray].list.map(el => el.asInstanceOf[Double]).toArray)
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
        if (datum == null) FastTypeTag.Null
        else if (hints.isElidedType) {
          datum match {
            case JSONObject(fields) if fields.contains("$ref") => FastTypeTag.Ref
            case _ => hints.tag
          }
        } else {
          datum match {
            case JSONObject(fields) if fields.contains("$ref") => FastTypeTag.Ref
            case JSONObject(fields) if fields.contains("tpe") => FastTypeTag(mirror, fields("tpe").asInstanceOf[String])
            case JSONObject(fields) => hints.tag
          }
        }
      }
      lastReadTag
    }
    def atPrimitive: Boolean = primitives.contains(lastReadTag.key)
    def readPrimitive(): Any = {
      datum match {
        case JSONArray(list) if lastReadTag.key != FastTypeTag.ArrayByte.key &&
                                lastReadTag.key != FastTypeTag.ArrayShort.key &&
                                lastReadTag.key != FastTypeTag.ArrayChar.key &&
                                lastReadTag.key != FastTypeTag.ArrayInt.key &&
                                lastReadTag.key != FastTypeTag.ArrayLong.key &&
                                lastReadTag.key != FastTypeTag.ArrayBoolean.key &&
                                lastReadTag.key != FastTypeTag.ArrayFloat.key &&
                                lastReadTag.key != FastTypeTag.ArrayDouble.key =>
          // now this is a hack!
          val value = mkNestedReader(list.head).primitives(lastReadTag.key)()
          datum = JSONArray(list.tail)
          value
        case JSONObject(fields) if lastReadTag.key != FastTypeTag.Ref.key =>
          mkNestedReader(fields("value")).primitives(lastReadTag.key)()
        case _ =>
          primitives(lastReadTag.key)()
      }
    }
    def atObject: Boolean = datum.isInstanceOf[JSONObject]
    def readField(name: String): JSONPickleReader = {
      datum match {
        case JSONObject(fields) => mkNestedReader(fields(name))
      }
    }
    def endEntry(): Unit = {}
    def beginCollection(): PReader = readField("elems")
    def readLength(): Int = {
      datum match {
        case JSONArray(list) => list.length
      }
    }
    private var i = 0
    def readElement(): PReader = {
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
