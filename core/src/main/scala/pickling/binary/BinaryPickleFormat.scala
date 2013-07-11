package scala.pickling

import scala.language.implicitConversions

import scala.reflect.runtime.universe.Mirror

package object binary {
  implicit val pickleFormat = new BinaryPickleFormat
  implicit def toBinaryPickle(value: Array[Byte]): BinaryPickle = BinaryPickle(value)
}

package binary {

  case class BinaryPickle(value: Array[Byte]) extends Pickle {
    type ValueType = Array[Byte]
    type PickleFormatType = BinaryPickleFormat
    override def toString = s"""BinaryPickle(${value.mkString("[", ",", "]")})"""
  }

  class BinaryPickleBuilder(format: BinaryPickleFormat, out: EncodingOutput[Array[Byte]]) extends PBuilder with PickleTools {
    import format._

    private var byteBuffer: EncodingOutput[Array[Byte]] =
      out.asInstanceOf[EncodingOutput[Array[Byte]]]

    private var pos = 0

    @inline private[this] def mkByteBuffer(knownSize: Int): Unit =
      if (byteBuffer == null) {
        byteBuffer = if (knownSize != -1) new ByteArray(knownSize) else new ByteArrayBuffer
      }

    def beginEntry(picklee: Any): this.type = withHints { hints =>
      mkByteBuffer(hints.knownSize)

      if (picklee == null) {
        pos = byteBuffer.encodeByteTo(pos, NULL_TAG)
      } else {
        def writeTpe() = {
          val tpe = hints.tag.tpe
          val tpeBytes = typeToString(tpe).getBytes("UTF-8")
          byteBuffer.encodeIntAtEnd(pos, tpeBytes.length)
          pos += 4
          pos = byteBuffer.copyTo(pos, tpeBytes)
        }

        pos = hints.tag.key match { // PERF: should store typestring once in hints.
          case KEY_NULL =>
            if (!hints.isElidedType) writeTpe()
            byteBuffer.encodeByteTo(pos, NULL_TAG)
          case KEY_BYTE =>
            if (!hints.isElidedType) writeTpe()
            byteBuffer.encodeByteAtEnd(pos, picklee.asInstanceOf[Byte])
            pos + 1
          case KEY_SHORT =>
            if (!hints.isElidedType) writeTpe()
            byteBuffer.encodeShortAtEnd(pos, picklee.asInstanceOf[Short])
            pos + 2
          case KEY_CHAR =>
            if (!hints.isElidedType) writeTpe()
            byteBuffer.encodeCharAtEnd(pos, picklee.asInstanceOf[Char])
            pos + 2
          case KEY_INT =>
            if (!hints.isElidedType) writeTpe()
            byteBuffer.encodeIntAtEnd(pos, picklee.asInstanceOf[Int])
            pos + 4
          case KEY_LONG =>
            if (!hints.isElidedType) writeTpe()
            byteBuffer.encodeLongAtEnd(pos, picklee.asInstanceOf[Long])
            pos + 8
          case KEY_BOOLEAN =>
            if (!hints.isElidedType) writeTpe()
            byteBuffer.encodeBooleanTo(pos, picklee.asInstanceOf[Boolean])
          case KEY_FLOAT =>
            if (!hints.isElidedType) writeTpe()
            val intValue = java.lang.Float.floatToRawIntBits(picklee.asInstanceOf[Float])
            byteBuffer.encodeIntAtEnd(pos, intValue)
            pos + 4
          case KEY_SCALA_STRING | KEY_JAVA_STRING =>
            if (!hints.isElidedType) writeTpe()
            byteBuffer.encodeStringTo(pos, picklee.asInstanceOf[String])
          case KEY_ARRAY_INT =>
            if (!hints.isElidedType) writeTpe()
            byteBuffer.encodeIntArrayTo(pos, picklee.asInstanceOf[Array[Int]])
          case _ =>
            if (hints.isElidedType) byteBuffer.encodeByteTo(pos, ELIDED_TAG)
            else { writeTpe(); pos }
        }
      }

      this
    }

    def putField(name: String, pickler: this.type => Unit): this.type = {
      // can skip writing name if we pickle/unpickle in the same order
      pickler(this)
      this
    }

    def endEntry(): Unit = { /* do nothing */ }

    var beginCollPos = 0

    def beginCollection(length: Int): this.type = {
      beginCollPos = pos
      byteBuffer.encodeIntAtEnd(pos, 0)
      pos += 4
      this
    }

    def putElement(pickler: this.type => Unit): this.type = {
      pickler(this)
      this
    }

    def endCollection(length: Int): Unit = {
      byteBuffer.encodeIntTo(beginCollPos, length)
    }

    def result() = {
      BinaryPickle(byteBuffer.result())
    }
  }

  class BinaryPickleReader(arr: Array[Byte], val mirror: Mirror, format: BinaryPickleFormat) extends PReader with PickleTools {
    import format._

    private val byteBuffer: ByteBuffer       = new ByteArray(arr)
    private var pos                          = 0
    private var _lastTagRead: FastTypeTag[_] = null
    private var _lastTypeStringRead: String  = null

    private def lastTagRead: FastTypeTag[_] =
      if (_lastTagRead != null)
        _lastTagRead
      else {
        // assume _lastTypeStringRead != null
        _lastTagRead = FastTypeTag(mirror, _lastTypeStringRead)
        _lastTagRead
      }

    def beginEntryNoTag(): String = {
      val res: Any = withHints { hints =>
        if (hints.isElidedType && nullablePrimitives.contains(hints.tag.key)) {
          val (lookahead, newpos) = byteBuffer.decodeByteFrom(pos)
          lookahead match {
            case NULL_TAG => pos = newpos; FastTypeTag.Null
            case _        => hints.tag
          }
        } else if (hints.isElidedType && primitives.contains(hints.tag.key)) {
          hints.tag
        } else {
          val (lookahead, newpos) = byteBuffer.decodeByteFrom(pos)
          lookahead match {
            case NULL_TAG =>
              pos = newpos
              FastTypeTag.Null
            case ELIDED_TAG =>
              pos = newpos
              hints.tag
            case _ =>
              val (typeString, newpos) = byteBuffer.decodeStringFrom(pos)
              pos = newpos
              typeString
          }
        }
      }
      if (res.isInstanceOf[String]) {
        _lastTagRead = null
        _lastTypeStringRead = res.asInstanceOf[String]
        _lastTypeStringRead
      } else {
        _lastTagRead = res.asInstanceOf[FastTypeTag[_]]
        _lastTagRead.key
      }
    }

    def beginEntry(): FastTypeTag[_] = {
      beginEntryNoTag()
      lastTagRead
    }

    def atPrimitive: Boolean = primitives.contains(lastTagRead.key)

    def readPrimitive(): Any = {
      val (res, newpos) = {
        lastTagRead.key match {
          case KEY_NULL    => (null, pos)
          case KEY_BYTE    => byteBuffer.decodeByteFrom(pos)
          case KEY_SHORT   => byteBuffer.decodeShortFrom(pos)
          case KEY_CHAR    => byteBuffer.decodeCharFrom(pos)
          case KEY_INT     => byteBuffer.decodeIntFrom(pos)
          case KEY_LONG    => byteBuffer.decodeLongFrom(pos)
          case KEY_BOOLEAN => byteBuffer.decodeBooleanFrom(pos)
          case KEY_FLOAT   =>
            val (r, np) = byteBuffer.decodeIntFrom(pos)
            (java.lang.Float.intBitsToFloat(r), np)
          case KEY_SCALA_STRING | KEY_JAVA_STRING => byteBuffer.decodeStringFrom(pos)
          case KEY_ARRAY_INT => byteBuffer.decodeIntArrayFrom(pos)
        }
      }
      pos = newpos
      res
    }

    def atObject: Boolean = !atPrimitive

    def readField(name: String): BinaryPickleReader =
      this

    def endEntry(): Unit = { /* do nothing */ }

    def beginCollection(): PReader = this

    def readLength(): Int = {
      val (length, newpos) = byteBuffer.decodeIntFrom(pos)
      pos = newpos
      length
    }

    def readElement(): PReader = this

    def endCollection(): Unit = { /* do nothing */ }
  }

  class BinaryPickleFormat extends PickleFormat {
    val ELIDED_TAG: Byte = -1
    val NULL_TAG: Byte = -2

    val KEY_NULL    = FastTypeTag.Null.key
    val KEY_BYTE    = FastTypeTag.Byte.key
    val KEY_SHORT   = FastTypeTag.Short.key
    val KEY_CHAR    = FastTypeTag.Char.key
    val KEY_INT     = FastTypeTag.Int.key
    val KEY_LONG    = FastTypeTag.Long.key
    val KEY_BOOLEAN = FastTypeTag.Boolean.key
    val KEY_FLOAT   = FastTypeTag.Float.key
    val KEY_DOUBLE  = FastTypeTag.Double.key
    val KEY_UNIT    = FastTypeTag.Unit.key

    val KEY_SCALA_STRING = FastTypeTag.ScalaString.key
    val KEY_JAVA_STRING  = FastTypeTag.JavaString.key

    val KEY_ARRAY_BYTE   = FastTypeTag.ArrayByte.key
    val KEY_ARRAY_INT    = FastTypeTag.ArrayInt.key
    val KEY_ARRAY_LONG   = FastTypeTag.ArrayLong.key

    val primitives = Set(KEY_NULL, KEY_BYTE, KEY_SHORT, KEY_CHAR, KEY_INT, KEY_LONG, KEY_BOOLEAN, KEY_FLOAT, KEY_DOUBLE, KEY_UNIT, KEY_SCALA_STRING, KEY_JAVA_STRING, KEY_ARRAY_BYTE, KEY_ARRAY_INT, KEY_ARRAY_LONG)
    val nullablePrimitives = Set(KEY_NULL, KEY_SCALA_STRING, KEY_JAVA_STRING, KEY_ARRAY_BYTE, KEY_ARRAY_INT, KEY_ARRAY_LONG)

    type PickleType = BinaryPickle
    type OutputType = EncodingOutput[Array[Byte]]
    def createBuilder() = new BinaryPickleBuilder(this, null)
    def createBuilder(out: EncodingOutput[Array[Byte]]): PBuilder = new BinaryPickleBuilder(this, out)
    def createReader(pickle: PickleType, mirror: Mirror) = new BinaryPickleReader(pickle.value, mirror, this)
  }
}
