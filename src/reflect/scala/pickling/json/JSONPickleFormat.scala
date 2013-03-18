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
  import ir._

  case class JSONPickle(value: String) extends Pickle {
    type ValueType = String
    type PickleFormatType = JSONPickleFormat
  }

  class JSONPickleFormat extends PickleFormat {
    type PickleType = JSONPickle

    type PickleBuilderType = JSONPickleBuilder
    def createBuilder() = new JSONPickleBuilder

    type PickleReaderType = JSONPickleReader
    def createReader(pickle: JSONPickle) = {
      JSON.parseRaw(pickle.value) match {
        case Some(raw) => new JSONPickleReader(raw)
        case None => throw new PicklingException("failed to parse \"" + pickle.value + "\" as JSON")
      }
    }
  }

  class JSONPickleBuilder extends PickleBuilder {
    type PickleFormatType = JSONPickleFormat
    implicit val format = json.pickleFormat
    type PickleType = JSONPickle

    private val buf = new StringBuilder()
    private val stack = new Stack[Type]()
    private def isJSONPrimitive(tpe: Type) = {
      val sym = tpe.typeSymbol.asClass
      sym == NullClass || sym.isPrimitive || sym == StringClass
    }

    def beginEntry(tpe: Type, picklee: Any): this.type = {
      stack.push(tpe)
      val sym = tpe.typeSymbol.asClass
      if (isJSONPrimitive(tpe)) {
        if (sym == NullClass) buf ++= "null"
        else if (sym == CharClass || sym == StringClass) buf ++= "\"" + JSONFormat.quoteString(picklee.toString) + "\""
        else buf ++= picklee.toString // TODO: unit?
      } else {
        buf ++= "{\n"
        def pickleTpe(tpe: Type): String = {
          def loop(tpe: Type): String = tpe match {
            case TypeRef(_, sym, Nil) => s"${sym.fullName}" + (if (sym.isModuleClass) "$" else "")
            case TypeRef(_, sym, targs) => loop(tpe.typeConstructor) + s"[${targs.map(targ => pickleTpe(targ))}]"
          }
          "  \"tpe\": \"" + loop(tpe) + "\""
        }
        buf ++= pickleTpe(tpe)
      }
      this
    }
    def putField(name: String, pickler: this.type => Unit): this.type = {
      assert(!isJSONPrimitive(stack.top), stack.top)
      buf ++= ",\n  \"" + name + "\": "
      pickler(this)
      this
    }
    def endEntry(): Unit = {
      val tpe = stack.pop()
      if (isJSONPrimitive(tpe)) () // do nothing
      else buf ++= "\n}"
    }
    def result(): JSONPickle = {
      assert(stack.isEmpty, stack)
      JSONPickle(buf.toString)
    }
  }

  class JSONPickleReader(datum: Any) extends PickleReader {
    type PickleFormatType = JSONPickleFormat
    implicit val format = json.pickleFormat
    def readType(mirror: Mirror): Type = {
      def unpickleTpe(stpe: String): Type = {
        // TODO: support polymorphic types as serialized above with pickleTpe
        val sym = if (stpe.endsWith("$")) mirror.staticModule(stpe.stripSuffix("$")).moduleClass else mirror.staticClass(stpe)
        sym.asType.toType
      }
      datum match {
        case JSONObject(fields) => unpickleTpe(fields("tpe").asInstanceOf[String])
        case JSONArray(elements) => throw new PicklingException(s"TODO: not yet implemented ($datum)")
        case _: String => StringClass.toType
        case _: Double => DoubleClass.toType
        case null => NullTpe
      }
    }
    def atPrimitive: Boolean = !atObject
    def readPrimitive(tpe: Type): Any = {
      tpe match {
        case tpe if tpe =:= StringClass.toType => datum.asInstanceOf[String]
        case tpe if tpe =:= IntClass.toType => datum.asInstanceOf[Double].toInt
      }
    }
    def atObject: Boolean = datum.isInstanceOf[JSONObject]
    def readField(name: String): JSONPickleReader = {
      datum match {
        case JSONObject(fields) => new JSONPickleReader(fields(name)).asInstanceOf[this.type] // TODO: think this over
      }
    }
  }
}
