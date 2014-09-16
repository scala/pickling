package scala.pickling

import scala.pickling.internal._
import scala.language.implicitConversions
import scala.reflect.runtime.universe.Mirror

import java.io.InputStream

package object binary {
  implicit val pickleFormat = new BinaryPickleFormat
  implicit def toBinaryPickle(value: Array[Byte]): BinaryPickle = BinaryPickle(value)
}

package binary {

  abstract class BinaryPickle extends Pickle {
    type PickleFormatType = BinaryPickleFormat
    type ValueType = Array[Byte]

    val value: Array[Byte]

    def createReader(mirror: Mirror, format: BinaryPickleFormat): PReader
  }

  case class BinaryPickleArray(data: Array[Byte]) extends BinaryPickle {
    val value: Array[Byte] = data

    def createReader(mirror: Mirror, format: BinaryPickleFormat): PReader =
      new BinaryPickleReader(data, mirror, format)

    override def toString = s"""BinaryPickle(${value.mkString("[", ",", "]")})"""
  }

  case class BinaryPickleStream(input: InputStream) extends BinaryPickle {
    val value: Array[Byte] = Array.ofDim[Byte](0)

    def createReader(mirror: Mirror, format: BinaryPickleFormat): PReader =
      new BinaryInputStreamReader(input, mirror, format)

    /* Do not override def toString to avoid traversing the input stream. */
  }

  object BinaryPickle {
    def apply(a: Array[Byte]): BinaryPickle =
      new BinaryPickleArray(a)
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
        if (!hints.isElidedType) {
          // quickly decide whether we should use picklee.getClass instead
          val ts =
            if (hints.tag.key.contains("anonfun$")) picklee.getClass.getName
            else hints.tag.key
          Util.encodeString(output, ts)
        }

        // NOTE: it looks like we don't have to write object ids at all
        // traversals employed by pickling and unpickling are exactly the same
        // hence when unpickling it's enough to just increment the nextUnpicklee counter
        // and everything will work out automatically!

        hints.tag.key match { // PERF: should store typestring once in hints.
          case KEY_UNIT =>
            Util.encodeByte(output, UNIT_TAG)
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
          case KEY_STRING =>
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

  abstract class AbstractBinaryReader(val mirror: Mirror) {
    protected var _lastTagRead: FastTypeTag[_] = null
    protected var _lastTypeStringRead: String  = null

    protected def lastTagRead: FastTypeTag[_] =
      if (_lastTagRead != null)
        _lastTagRead
      else {
        // assume _lastTypeStringRead != null
        _lastTagRead = FastTypeTag(mirror, _lastTypeStringRead)
        _lastTagRead
      }
  }

  class BinaryInputStreamReader(in: InputStream, mirror: Mirror, format: BinaryPickleFormat) extends AbstractBinaryReader(mirror) with PReader with PickleTools {
    import format._

    def nextByte(): Byte = {
      val b = in.read()
      if (b == -1) throw new EndOfStreamException
      b.asInstanceOf[Byte]
    }

    def decodeStringWithLookahead(la: Byte): String = {
      // read 3 more bytes
      val buf = Array[Byte](la, nextByte(), nextByte(), nextByte())
      val len = {
        val len0 = Util.decodeIntFrom(buf, 0)
        if (len0 > 1000)
          throw PicklingException(s"decodeStringWithLookahead: corrupted length of type string: $len0")
        else if (len0 < 0)
          throw PicklingException(s"decodeStringWithLookahead: negative length of type string: $len0\nbuf: [${buf.mkString(",")}]")
        else
          len0
      }
      val bytes = Array.ofDim[Byte](len)
      var num = in.read(bytes)
      while (num < len) {
        val readMore = in.read(bytes, num, len - num)
        num += readMore
      }
      new String(bytes, "UTF-8")
    }

    var gla: Option[Byte] = None

    def beginEntryNoTag(): String =
      beginEntryNoTagDebug(false)

    def beginEntryNoTagDebug(debugOn: Boolean): String = {
      val res: Any = withHints { hints =>
        // if (debugOn)
        //   debug(s"hints: $hints")

        if (hints.isElidedType && nullablePrimitives.contains(hints.tag.key)) {
          val lookahead = nextByte()
          lookahead match {
            case NULL_TAG => gla = Some(lookahead); FastTypeTag.Null
            case REF_TAG  => FastTypeTag.Ref
            case _        => gla = Some(lookahead); hints.tag
          }
        } else if (hints.isElidedType && primitives.contains(hints.tag.key)) {
          hints.tag
        } else {
          val lookahead = nextByte()
          // if (debugOn)
          //   debug(s"checking lookahead: $lookahead")
          lookahead match {
            case NULL_TAG =>
              FastTypeTag.Null
            case ELIDED_TAG =>
              hints.tag
            case REF_TAG =>
              FastTypeTag.Ref
            case _ =>
              // do not consume lookahead byte
              val res = try {
                decodeStringWithLookahead(lookahead)
              } catch {
                case PicklingException(msg) =>
                  val primInfo = if (hints.tag == null) ""
                    else s"\nnullable prim: ${nullablePrimitives.contains(hints.tag.key)}\nprim: ${primitives.contains(hints.tag.key)}"
                  throw PicklingException(s"error decoding type string. debug info: $hints$primInfo\ncause:$msg")
              }
              // if (debugOn)
              //   debug(s"decodeStringWithLookahead: $res")
              res
          }
        }
      }
      if (res.isInstanceOf[String]) {
        // if (debugOn)
        //   debug(s"replacing tag with last type string read: ${res.asInstanceOf[String]}")
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

    def decodeInt(): Int = {
      val buf = Array[Byte](nextByte(), nextByte(), nextByte(), nextByte())
      Util.decodeIntFrom(buf, 0)
    }

    def decodeIntWithLookahead(): Int = gla match {
      case Some(fstByte) =>
        gla = None // clear global lookahead
        val buf = Array[Byte](fstByte, nextByte(), nextByte(), nextByte())
        Util.decodeIntFrom(buf, 0)
      case None =>
        decodeInt()
    }

    def decodeShort(): Short = {
      val buf = Array[Byte](nextByte(), nextByte())
      val fst = ((buf(0) << 8) & 0xFFFF).toShort
      val snd = (buf(1)        & 0x00FF).toShort
      (fst | snd).toShort
    }

    def decodeChar(): Char = {
      val buf = Array[Byte](nextByte(), nextByte())
      val fst = ((buf(0) << 8) & 0xFFFF).toChar
      val snd = (buf(1)        & 0x00FF).toChar
      (fst | snd).toChar
    }

    def decodeLong(): Long = {
      val buf = Array[Byte](nextByte(), nextByte(), nextByte(), nextByte(),
                            nextByte(), nextByte(), nextByte(), nextByte())
      val elem1 = ((buf(0).toLong << 56) & 0xFFFFFFFFFFFFFFFFL).toLong
      val elem2 = ((buf(1).toLong << 48) & 0x00FFFFFFFFFFFFFFL).toLong
      val elem3 = ((buf(2).toLong << 40) & 0x0000FFFFFFFFFFFFL).toLong
      val elem4 = ((buf(3).toLong << 32) & 0x000000FFFFFFFFFFL).toLong
      val elem5 = ((buf(4).toLong << 24) & 0x00000000FFFFFFFFL).toLong
      val elem6 = ((buf(5).toLong << 16) & 0x0000000000FFFFFFL).toLong
      val elem7 = ((buf(6).toLong << 8)  & 0x000000000000FFFFL).toLong
      val elem8 = (buf(7).toLong         & 0x00000000000000FFL).toLong
      elem1 | elem2 | elem3 | elem4 | elem5 | elem6 | elem7 | elem8
    }

    def decodeBoolean(): Boolean = {
      nextByte() != 0
    }

    def decodeString(): String = {
      val len = decodeIntWithLookahead()
      val bytes = Array.ofDim[Byte](len)
      if (len > 0) {
        val num = in.read(bytes)
        if (num < len) throw new Exception("Could not read enough bytes from input stream")
      }
      new String(bytes, "UTF-8")
    }

    def decodeByteArray(): Array[Byte] = {
      val len = decodeIntWithLookahead()
      val arr = Array.ofDim[Byte](len)
      in.read(arr)
      Util.decodeByteArray(arr, 0, len)
    }

    def decodeShortArray(): Array[Short] = {
      val len = decodeIntWithLookahead()
      val arr = Array.ofDim[Byte](len * 2)
      in.read(arr)
      Util.decodeShortArray(arr, 0, len)
    }

    def decodeCharArray(): Array[Char] = {
      val len = decodeIntWithLookahead()
      val arr = Array.ofDim[Byte](len * 2)
      in.read(arr)
      Util.decodeCharArray(arr, 0, len)
    }

    def decodeIntArray(): Array[Int] = {
      val len = decodeIntWithLookahead()
      val arr = Array.ofDim[Byte](len * 4)
      in.read(arr)
      Util.decodeIntArray(arr, 0, len)
    }

    // Consider a macro such as Util.decodeArray[Long](in, len)
    def decodeLongArray(): Array[Long] = {
      val len = decodeIntWithLookahead()
      val arr = Array.ofDim[Byte](len * 8)
      in.read(arr)
      Util.decodeLongArray(arr, 0, len)
    }

    def decodeBooleanArray(): Array[Boolean] = {
      val len = decodeIntWithLookahead()
      val arr = Array.ofDim[Byte](len)
      in.read(arr)
      Util.decodeBooleanArray(arr, 0, len)
    }

    def decodeFloatArray(): Array[Float] = {
      val len = decodeIntWithLookahead()
      val arr = Array.ofDim[Byte](len * 4)
      in.read(arr)
      Util.decodeFloatArray(arr, 0, len)
    }

    def decodeDoubleArray(): Array[Double] = {
      val len = decodeIntWithLookahead()
      val arr = Array.ofDim[Byte](len * 8)
      in.read(arr)
      Util.decodeDoubleArray(arr, 0, len)
    }

    def readPrimitive(): Any = {
      val res = lastTagRead.key match {
        case KEY_NULL    => null
        case KEY_REF     => lookupUnpicklee(decodeInt())
        case KEY_BYTE    => nextByte()
        case KEY_SHORT   => decodeShort()
        case KEY_CHAR    => decodeChar()
        case KEY_INT     => decodeInt()
        case KEY_LONG    => decodeLong()
        case KEY_BOOLEAN => decodeBoolean()
        case KEY_FLOAT   =>
          val r = decodeInt()
          java.lang.Float.intBitsToFloat(r)
        case KEY_DOUBLE  =>
          val r = decodeLong()
          java.lang.Double.longBitsToDouble(r)

        case KEY_STRING => decodeString()

        case KEY_ARRAY_BYTE    => decodeByteArray()
        case KEY_ARRAY_SHORT   => decodeShortArray()
        case KEY_ARRAY_CHAR    => decodeCharArray()
        case KEY_ARRAY_INT     => decodeIntArray()
        case KEY_ARRAY_LONG    => decodeLongArray()
        case KEY_ARRAY_BOOLEAN => decodeBooleanArray()
        case KEY_ARRAY_FLOAT   => decodeFloatArray()
        case KEY_ARRAY_DOUBLE  => decodeDoubleArray()
      }
      res
    }

    def atObject: Boolean = !atPrimitive

    def readField(name: String): BinaryInputStreamReader =
      this

    def endEntry(): Unit = { /* do nothing */ }

    def beginCollection(): PReader = this

    def readLength(): Int = {
      decodeInt()
    }

    def readElement(): PReader = this

    def endCollection(): Unit = { /* do nothing */ }
  }

  class BinaryPickleReader(arr: Array[Byte], mirror: Mirror, format: BinaryPickleFormat) extends AbstractBinaryReader(mirror) with PReader with PickleTools {
    import format._

    private var pos = 0

    def beginEntryNoTag(): String =
      beginEntryNoTagDebug(false)

    def beginEntryNoTagDebug(debugOn: Boolean): String = {
      val res: Any = withHints { hints =>
        if (hints.isElidedType && nullablePrimitives.contains(hints.tag.key)) {
          val lookahead = arr(pos)
          lookahead match {
            case UNIT_TAG => pos += 1; FastTypeTag.Unit
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
          case KEY_UNIT    => ()
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

          case KEY_STRING => val r = Util.decodeStringFrom(arr, pos); newpos = r._2 ; r._1

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
    val NULL_TAG  : Byte = -2
    val REF_TAG   : Byte = -3
    val UNIT_TAG  : Byte = -4
    val ELIDED_TAG: Byte = -5

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

    val KEY_STRING  = FastTypeTag.String.key

    val KEY_ARRAY_BYTE    = FastTypeTag.ArrayByte.key
    val KEY_ARRAY_SHORT   = FastTypeTag.ArrayShort.key
    val KEY_ARRAY_CHAR    = FastTypeTag.ArrayChar.key
    val KEY_ARRAY_INT     = FastTypeTag.ArrayInt.key
    val KEY_ARRAY_LONG    = FastTypeTag.ArrayLong.key
    val KEY_ARRAY_BOOLEAN = FastTypeTag.ArrayBoolean.key
    val KEY_ARRAY_FLOAT   = FastTypeTag.ArrayFloat.key
    val KEY_ARRAY_DOUBLE  = FastTypeTag.ArrayDouble.key

    val KEY_REF = FastTypeTag.Ref.key

    val primitives = Set(KEY_NULL, KEY_REF, KEY_BYTE, KEY_SHORT, KEY_CHAR, KEY_INT, KEY_LONG, KEY_BOOLEAN, KEY_FLOAT, KEY_DOUBLE, KEY_UNIT, KEY_STRING, KEY_ARRAY_BYTE, KEY_ARRAY_SHORT, KEY_ARRAY_CHAR, KEY_ARRAY_INT, KEY_ARRAY_LONG, KEY_ARRAY_BOOLEAN, KEY_ARRAY_FLOAT, KEY_ARRAY_DOUBLE)
    val nullablePrimitives = Set(KEY_NULL, KEY_STRING, KEY_ARRAY_BYTE, KEY_ARRAY_SHORT, KEY_ARRAY_CHAR, KEY_ARRAY_INT, KEY_ARRAY_LONG, KEY_ARRAY_BOOLEAN, KEY_ARRAY_FLOAT, KEY_ARRAY_DOUBLE)

    type PickleType = BinaryPickle
    type OutputType = ArrayOutput[Byte]
    def createBuilder() = new BinaryPickleBuilder(this, null)
    def createBuilder(out: ArrayOutput[Byte]): PBuilder = new BinaryPickleBuilder(this, out)
    def createReader(pickle: PickleType, mirror: Mirror) = pickle.createReader(mirror, this)
  }
}
