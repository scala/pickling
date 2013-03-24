package scala.pickling

package object binary {
  implicit val pickleFormat = new BinaryPickleFormat
  implicit def toBinaryPickle(value: Array[Byte]): BinaryPickle = BinaryPickle(value)
}

package binary {
  import scala.reflect.runtime.{universe => ru}

  case class BinaryPickle(value: Array[Byte]) extends Pickle {
    type ValueType = Array[Byte]
    type PickleFormatType = BinaryPickleFormat
  }

  class BinaryPickleBuilder(format: BinaryPickleFormat) extends PickleBuilder {
    import ru._
    import definitions._

    private var byteBuffer: ByteBuffer = _
    private var pos = 0

    private def formatType(tpe: Type): Array[Byte] = {
      val s = tpe match {
        case TypeRef(_, sym, Nil) => s"${sym.fullName}"
        case TypeRef(_, sym, targs) => ???
      }
      s.getBytes("UTF-8")
    }

    private def mkByteBuffer(knownSize: Int) = {
      if (byteBuffer == null) {
        byteBuffer =
          if (knownSize != -1) new ByteArray(knownSize)
          else new ByteArrayBuffer
      }
    }

    private def encodePicklee(sym: Symbol, picklee: Any): Int = {
      if (sym == IntClass) byteBuffer.encodeIntTo(pos, picklee.asInstanceOf[Int])
      else if (sym == StringClass) byteBuffer.encodeStringTo(pos, picklee.asInstanceOf[String])
      else if (sym == BooleanClass) byteBuffer.encodeBooleanTo(pos, picklee.asInstanceOf[Boolean])
      else ???
    }

    def beginEntryNoType(tag: TypeTag[_], picklee: Any, knownSize: Int): this.type = {
      mkByteBuffer(knownSize)

      // in the "NoType" version we are very careful not to call .tpe on the tag if not needed
      // the tag can be compared against tags for primitive types as-is
      if (tag == TypeTag.Int)
        pos = byteBuffer.encodeIntTo(pos, picklee.asInstanceOf[Int])
      else if (tag == TypeTag.Boolean)
        pos = byteBuffer.encodeBooleanTo(pos, picklee.asInstanceOf[Boolean])
      else {
        val tpe = tag.tpe
        if (format.isPrimitive(tpe)) {
          if (tpe.typeSymbol.asClass == StringClass)
            pos = byteBuffer.encodeStringTo(pos, picklee.asInstanceOf[String])
          else
            ???
        }
      }

      this
    }

    def beginEntry(tag: TypeTag[_], picklee: Any, knownSize: Int): this.type = {
      val tpe = tag.tpe
      mkByteBuffer(knownSize)

      // write pickled tpe to `target`:
      // length of pickled type, pickled type
      val tpeBytes = formatType(tpe)
      pos = byteBuffer.encodeIntTo(pos, tpeBytes.length)
      pos = byteBuffer.copyTo(pos, tpeBytes)

      if (format.isPrimitive(tpe)) {
        val sym = tpe.typeSymbol.asClass
        pos = encodePicklee(sym, picklee)
      }

      this
    }

    def putField(name: String, pickler: this.type => Unit): this.type = {
      // can skip writing name if we pickle/unpickle in the same order
      pickler(this)
      this
    }

    def endEntry(): Unit = { /* do nothing */ }

    def result() = {
      BinaryPickle(byteBuffer.toArray)
    }
  }

  class BinaryPickleReader(arr: Array[Byte], format: BinaryPickleFormat) extends PickleReader {
    import ru._

    private val byteBuffer: ByteBuffer = new ByteArray(arr)
    private var pos = 0
    private var atPrim = false

    def readTag(mirror: Mirror): TypeTag[_] = {
      def readType = {
        def unpickleTpe(stpe: String): Type = {
          // TODO: support polymorphic types as serialized above with pickleTpe
          mirror.staticClass(stpe).asType.toType
        }
        val (typeString, newpos) = byteBuffer.decodeStringFrom(pos)
        pos = newpos
        val tpe = unpickleTpe(typeString)
        atPrim = format.isPrimitive(tpe)
        tpe
      }
      TypeTag(readType)
    }

    def atPrimitive: Boolean = atPrim

    def readPrimitive(tag: TypeTag[_]): Any = {
      val tpe = tag.tpe
      val (res, newpos) =
        if      (tpe =:= typeOf[Int])     byteBuffer.decodeIntFrom(pos)
        else if (tpe =:= typeOf[String])  byteBuffer.decodeStringFrom(pos)
        else if (tpe =:= typeOf[Boolean]) byteBuffer.decodeBooleanFrom(pos)
        else ???
      pos = newpos
      res
    }

    def atObject: Boolean = !atPrimitive

    def readField(name: String): BinaryPickleReader =
      this
  }

  class BinaryPickleFormat extends PickleFormat {
    import ru._
    import definitions._

    type PickleType = BinaryPickle
    def createBuilder() = new BinaryPickleBuilder(this)
    def createReader(pickle: PickleType) = new BinaryPickleReader(pickle.value, this)

    def isPrimitive(tpe: Type) = {
      val sym = tpe.typeSymbol.asClass
      sym == IntClass || sym == StringClass || sym == BooleanClass
    }
  }

}
