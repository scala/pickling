package scala.pickling

import scala.pickling.internal._
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

  final class BinaryPickleBuilder(format: BinaryPickleFormat, out: ArrayOutput[Byte]) extends PBuilder with PickleTools {
    import format._

    private var output: ArrayOutput[Byte] = out

    @inline private[this] def mkOutput(knownSize: Int): Unit =
      if (output == null)
        output = if (knownSize != -1) new ByteArrayOutput(knownSize)
                 else new ByteArrayBufferOutput

    @inline def beginEntry(picklee: Any): PBuilder = withHints { hints =>
      mkOutput(hints.knownSize)

      if (picklee == null) {
        Util.encodeByte(output, NULL_TAG)
      } else if (hints.oid != -1) {
        Util.encodeByte(output, REF_TAG)
        Util.encodeInt(output, hints.oid)
      } else {
        if (!hints.isElidedType)
          Util.encodeString(output, hints.tag.key)

        // NOTE: it looks like we don't have to write object ids at all
        // traversals employed by pickling and unpickling are exactly the same
        // hence when unpickling it's enough to just increment the nextUnpicklee counter
        // and everything will work out automatically!

        hints.tag.key match { // PERF: should store typestring once in hints.
          case KEY_NULL =>
            Util.encodeByte(output, NULL_TAG)
          case KEY_BYTE =>
            Util.encodeByte(output, picklee.asInstanceOf[Byte])
          case KEY_SHORT =>
            Util.encodeShort(output, picklee.asInstanceOf[Short])
          case KEY_CHAR =>
            Util.encodeChar(output, picklee.asInstanceOf[Char])
          case KEY_INT =>
            Util.encodeInt(output, picklee.asInstanceOf[Int])
          case KEY_LONG =>
            Util.encodeLong(output, picklee.asInstanceOf[Long])
          case KEY_BOOLEAN =>
            Util.encodeBoolean(output, picklee.asInstanceOf[Boolean])
          case KEY_FLOAT =>
            val intValue = java.lang.Float.floatToRawIntBits(picklee.asInstanceOf[Float])
            Util.encodeInt(output, intValue)
          case KEY_DOUBLE =>
            val longValue = java.lang.Double.doubleToRawLongBits(picklee.asInstanceOf[Double])
            Util.encodeLong(output, longValue)
          case KEY_SCALA_STRING | KEY_JAVA_STRING =>
            Util.encodeString(output, picklee.asInstanceOf[String])
          case KEY_ARRAY_BYTE =>
            Util.encodeByteArray(output, picklee.asInstanceOf[Array[Byte]])
          case KEY_ARRAY_CHAR =>
            Util.encodeCharArray(output, picklee.asInstanceOf[Array[Char]])
          case KEY_ARRAY_SHORT =>
            Util.encodeShortArray(output, picklee.asInstanceOf[Array[Short]])
          case KEY_ARRAY_INT =>
            Util.encodeIntArray(output, picklee.asInstanceOf[Array[Int]])
          case KEY_ARRAY_LONG =>
            Util.encodeLongArray(output, picklee.asInstanceOf[Array[Long]])
          case KEY_ARRAY_BOOLEAN =>
            Util.encodeBooleanArray(output, picklee.asInstanceOf[Array[Boolean]])
          case KEY_ARRAY_FLOAT =>
            Util.encodeFloatArray(output, picklee.asInstanceOf[Array[Float]])
          case KEY_ARRAY_DOUBLE =>
            Util.encodeDoubleArray(output, picklee.asInstanceOf[Array[Double]])
          case _ =>
            if (hints.isElidedType) Util.encodeByte(output, ELIDED_TAG)
        }
      }
      this
    }

    @inline def putField(name: String, pickler: PBuilder => Unit): PBuilder = {
      // can skip writing name if we pickle/unpickle in the same order
      pickler(this)
      this
    }

    @inline def endEntry(): Unit = { /* do nothing */ }

    @inline def beginCollection(length: Int): PBuilder = {
      Util.encodeInt(output, length)
      this
    }

    @inline def putElement(pickler: PBuilder => Unit): PBuilder = {
      pickler(this)
      this
    }

    @inline def endCollection(): Unit = {
    }

    @inline def result() = {
      BinaryPickle(output.result())
    }
  }

  class BinaryPickleReader(arr: Array[Byte], val mirror: Mirror, format: BinaryPickleFormat) extends PReader with PickleTools {
    import format._

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
          val lookahead = arr(pos)
          lookahead match {
            case NULL_TAG => pos += 1; FastTypeTag.Null
            case REF_TAG  => pos += 1; FastTypeTag.Ref
            case _        => hints.tag
          }
        } else if (hints.isElidedType && primitives.contains(hints.tag.key)) {
          hints.tag
        } else {
          val lookahead = arr(pos)
          lookahead match {
            case NULL_TAG =>
              pos += 1
              FastTypeTag.Null
            case ELIDED_TAG =>
              pos += 1
              hints.tag
            case REF_TAG =>
              pos += 1
              FastTypeTag.Ref
            case _ =>
              val (typeString, newpos) = Util.decodeStringFrom(arr, pos)
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
      var newpos = pos
      val res = lastTagRead.key match {
          case KEY_NULL    => null
          case KEY_REF     => newpos = pos+4 ; lookupUnpicklee(Util.decodeIntFrom(arr, pos))
          case KEY_BYTE    => newpos = pos+1 ; arr(pos)
          case KEY_SHORT   => newpos = pos+2 ; Util.decodeShortFrom(arr, pos)
          case KEY_CHAR    => newpos = pos+2 ; Util.decodeCharFrom(arr, pos)
          case KEY_INT     => newpos = pos+4 ; Util.decodeIntFrom(arr, pos)
          case KEY_LONG    => newpos = pos+8 ; Util.decodeLongFrom(arr, pos)
          case KEY_BOOLEAN => newpos = pos+1 ; Util.decodeBooleanFrom(arr, pos)
          case KEY_FLOAT   =>
            val r = Util.decodeIntFrom(arr, pos)
            newpos = pos+4
            java.lang.Float.intBitsToFloat(r)
          case KEY_DOUBLE  =>
            val r = Util.decodeLongFrom(arr, pos)
            newpos = pos+8
            java.lang.Double.longBitsToDouble(r)

          case KEY_SCALA_STRING | KEY_JAVA_STRING => val r = Util.decodeStringFrom(arr, pos); newpos = r._2 ; r._1

          case KEY_ARRAY_BYTE => val r = Util.decodeByteArrayFrom(arr, pos); newpos = r._2 ; r._1
          case KEY_ARRAY_SHORT => val r = Util.decodeShortArrayFrom(arr, pos); newpos = r._2 ; r._1
          case KEY_ARRAY_CHAR => val r = Util.decodeCharArrayFrom(arr, pos); newpos = r._2 ; r._1
          case KEY_ARRAY_INT => val r = Util.decodeIntArrayFrom(arr, pos); newpos = r._2 ; r._1
          case KEY_ARRAY_LONG => val r = Util.decodeLongArrayFrom(arr, pos); newpos = r._2 ; r._1
          case KEY_ARRAY_BOOLEAN => val r = Util.decodeBooleanArrayFrom(arr, pos); newpos = r._2 ; r._1
          case KEY_ARRAY_FLOAT => val r = Util.decodeFloatArrayFrom(arr, pos); newpos = r._2 ; r._1
          case KEY_ARRAY_DOUBLE => val r = Util.decodeDoubleArrayFrom(arr, pos); newpos = r._2 ; r._1
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
      val length = Util.decodeIntFrom(arr, pos)
      pos += 4
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

    val primitives = Set(KEY_NULL, KEY_REF, KEY_BYTE, KEY_SHORT, KEY_CHAR, KEY_INT, KEY_LONG, KEY_BOOLEAN, KEY_FLOAT, KEY_DOUBLE, KEY_UNIT, KEY_SCALA_STRING, KEY_JAVA_STRING, KEY_ARRAY_BYTE, KEY_ARRAY_SHORT, KEY_ARRAY_CHAR, KEY_ARRAY_INT, KEY_ARRAY_LONG, KEY_ARRAY_BOOLEAN, KEY_ARRAY_FLOAT, KEY_ARRAY_DOUBLE)
    val nullablePrimitives = Set(KEY_NULL, KEY_SCALA_STRING, KEY_JAVA_STRING, KEY_ARRAY_BYTE, KEY_ARRAY_SHORT, KEY_ARRAY_CHAR, KEY_ARRAY_INT, KEY_ARRAY_LONG, KEY_ARRAY_BOOLEAN, KEY_ARRAY_FLOAT, KEY_ARRAY_DOUBLE)

    type PickleType = BinaryPickle
    type OutputType = ArrayOutput[Byte]
    def createBuilder() = new BinaryPickleBuilder(this, null)
    def createBuilder(out: ArrayOutput[Byte]): PBuilder = new BinaryPickleBuilder(this, out)
    def createReader(pickle: PickleType, mirror: Mirror) = new BinaryPickleReader(pickle.value, mirror, this)
  }
}
