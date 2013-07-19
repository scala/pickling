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
      } else if (hints.oid != -1) {
        byteBuffer.encodeByteTo(pos, REF_TAG)
        byteBuffer.encodeIntAtEnd(pos + 1, hints.oid)
        pos = pos + 5
      } else {
        if (!hints.isElidedType) {
          val tpeBytes = hints.tag.key.getBytes("UTF-8")
          byteBuffer.encodeIntAtEnd(pos, tpeBytes.length)
          pos += 4
          pos = byteBuffer.copyTo(pos, tpeBytes)
        }

        // NOTE: it looks like we don't have to write object ids at all
        // traversals employed by pickling and unpickling are exactly the same
        // hence when unpickling it's enough to just increment the nextUnpicklee counter
        // and everything will work out automatically!

        pos = hints.tag.key match { // PERF: should store typestring once in hints.
          case KEY_NULL =>
            byteBuffer.encodeByteTo(pos, NULL_TAG)
          case KEY_BYTE =>
            byteBuffer.encodeByteAtEnd(pos, picklee.asInstanceOf[Byte])
            pos + 1
          case KEY_SHORT =>
            byteBuffer.encodeShortAtEnd(pos, picklee.asInstanceOf[Short])
            pos + 2
          case KEY_CHAR =>
            byteBuffer.encodeCharAtEnd(pos, picklee.asInstanceOf[Char])
            pos + 2
          case KEY_INT =>
            byteBuffer.encodeIntAtEnd(pos, picklee.asInstanceOf[Int])
            pos + 4
          case KEY_LONG =>
            byteBuffer.encodeLongAtEnd(pos, picklee.asInstanceOf[Long])
            pos + 8
          case KEY_BOOLEAN =>
            byteBuffer.encodeBooleanTo(pos, picklee.asInstanceOf[Boolean])
          case KEY_FLOAT =>
            val intValue = java.lang.Float.floatToRawIntBits(picklee.asInstanceOf[Float])
            byteBuffer.encodeIntAtEnd(pos, intValue)
            pos + 4
          case KEY_DOUBLE =>
            val longValue = java.lang.Double.doubleToRawLongBits(picklee.asInstanceOf[Double])
            byteBuffer.encodeLongAtEnd(pos, longValue)
            pos + 8
          case KEY_SCALA_STRING | KEY_JAVA_STRING =>
            byteBuffer.encodeStringTo(pos, picklee.asInstanceOf[String])
          case KEY_ARRAY_BYTE =>
            byteBuffer.encodeByteArrayTo(pos, picklee.asInstanceOf[Array[Byte]])
          case KEY_ARRAY_CHAR =>
            byteBuffer.encodeCharArrayTo(pos, picklee.asInstanceOf[Array[Char]])
          case KEY_ARRAY_SHORT =>
            byteBuffer.encodeShortArrayTo(pos, picklee.asInstanceOf[Array[Short]])
          case KEY_ARRAY_INT =>
            byteBuffer.encodeIntArrayTo(pos, picklee.asInstanceOf[Array[Int]])
          case KEY_ARRAY_LONG =>
            byteBuffer.encodeLongArrayTo(pos, picklee.asInstanceOf[Array[Long]])
          case KEY_ARRAY_BOOLEAN =>
            byteBuffer.encodeBooleanArrayTo(pos, picklee.asInstanceOf[Array[Boolean]])
          case KEY_ARRAY_FLOAT =>
            byteBuffer.encodeFloatArrayTo(pos, picklee.asInstanceOf[Array[Float]])
          case KEY_ARRAY_DOUBLE =>
            byteBuffer.encodeDoubleArrayTo(pos, picklee.asInstanceOf[Array[Double]])
          case _ =>
            if (hints.isElidedType) byteBuffer.encodeByteTo(pos, ELIDED_TAG)
            else pos
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

    var beginCollPos = List[Int]()

    def beginCollection(length: Int): this.type = {
      beginCollPos = pos :: beginCollPos
      byteBuffer.encodeIntAtEnd(pos, 0)
      pos += 4
      this
    }

    def putElement(pickler: this.type => Unit): this.type = {
      pickler(this)
      this
    }

    def endCollection(length: Int): Unit = {
      val localBeginCollPos = beginCollPos.head
      beginCollPos = beginCollPos.tail
      byteBuffer.encodeIntTo(localBeginCollPos, length)
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
            case REF_TAG  => pos = newpos; FastTypeTag.Ref
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
            case REF_TAG =>
              pos = newpos
              FastTypeTag.Ref
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
          case KEY_REF     => (lookupUnpicklee(byteBuffer.decodeIntFrom(pos)._1), pos + 4)
          case KEY_BYTE    => byteBuffer.decodeByteFrom(pos)
          case KEY_SHORT   => byteBuffer.decodeShortFrom(pos)
          case KEY_CHAR    => byteBuffer.decodeCharFrom(pos)
          case KEY_INT     => byteBuffer.decodeIntFrom(pos)
          case KEY_LONG    => byteBuffer.decodeLongFrom(pos)
          case KEY_BOOLEAN => byteBuffer.decodeBooleanFrom(pos)
          case KEY_FLOAT   =>
            val (r, np) = byteBuffer.decodeIntFrom(pos)
            (java.lang.Float.intBitsToFloat(r), np)
          case KEY_DOUBLE  =>
            val (r, np) = byteBuffer.decodeLongFrom(pos)
            (java.lang.Double.longBitsToDouble(r), np)
          case KEY_SCALA_STRING | KEY_JAVA_STRING => byteBuffer.decodeStringFrom(pos)
          case KEY_ARRAY_BYTE => byteBuffer.decodeByteArrayFrom(pos)
          case KEY_ARRAY_SHORT => byteBuffer.decodeShortArrayFrom(pos)
          case KEY_ARRAY_CHAR => byteBuffer.decodeCharArrayFrom(pos)
          case KEY_ARRAY_INT => byteBuffer.decodeIntArrayFrom(pos)
          case KEY_ARRAY_LONG => byteBuffer.decodeLongArrayFrom(pos)
          case KEY_ARRAY_BOOLEAN => byteBuffer.decodeBooleanArrayFrom(pos)
          case KEY_ARRAY_FLOAT => byteBuffer.decodeFloatArrayFrom(pos)
          case KEY_ARRAY_DOUBLE => byteBuffer.decodeDoubleArrayFrom(pos)
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
    val REF_TAG: Byte = -3

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

    val KEY_ARRAY_BYTE    = FastTypeTag.ArrayByte.key
    val KEY_ARRAY_SHORT   = FastTypeTag.ArrayShort.key
    val KEY_ARRAY_CHAR    = FastTypeTag.ArrayChar.key
    val KEY_ARRAY_INT     = FastTypeTag.ArrayInt.key
    val KEY_ARRAY_LONG    = FastTypeTag.ArrayLong.key
    val KEY_ARRAY_BOOLEAN = FastTypeTag.ArrayBoolean.key
    val KEY_ARRAY_FLOAT   = FastTypeTag.ArrayFloat.key
    val KEY_ARRAY_DOUBLE  = FastTypeTag.ArrayDouble.key

    val KEY_REF = FastTypeTag.Ref.key

    val primitives = Set(KEY_NULL, KEY_REF, KEY_BYTE, KEY_SHORT, KEY_CHAR, KEY_INT, KEY_LONG, KEY_BOOLEAN, KEY_FLOAT, KEY_DOUBLE, KEY_UNIT, KEY_SCALA_STRING, KEY_JAVA_STRING, KEY_ARRAY_BYTE, KEY_ARRAY_INT, KEY_ARRAY_LONG)
    val nullablePrimitives = Set(KEY_NULL, KEY_SCALA_STRING, KEY_JAVA_STRING, KEY_ARRAY_BYTE, KEY_ARRAY_SHORT, KEY_ARRAY_CHAR, KEY_ARRAY_INT, KEY_ARRAY_LONG, KEY_ARRAY_BOOLEAN, KEY_ARRAY_FLOAT, KEY_ARRAY_DOUBLE)

    type PickleType = BinaryPickle
    type OutputType = EncodingOutput[Array[Byte]]
    def createBuilder() = new BinaryPickleBuilder(this, null)
    def createBuilder(out: EncodingOutput[Array[Byte]]): PBuilder = new BinaryPickleBuilder(this, out)
    def createReader(pickle: PickleType, mirror: Mirror) = new BinaryPickleReader(pickle.value, mirror, this)
  }
}
