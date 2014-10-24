package scala.pickling.binary

import scala.pickling._
import scala.pickling.internal._
import scala.language.implicitConversions
import scala.reflect.runtime.universe.Mirror

import java.io.InputStream

abstract class BinaryPickle extends Pickle {
  type PickleFormatType = BinaryPickleFormat
  type ValueType = Array[Byte]

  val value: Array[Byte]

  def createReader(mirror: Mirror, format: BinaryPickleFormat): PReader
}

case class BinaryPickleArray(data: Array[Byte]) extends BinaryPickle {
  val value: Array[Byte] = data

  def createReader(mirror: Mirror, format: BinaryPickleFormat): PReader =
    new BinaryPickleReader(new ByteArrayInput(data), mirror, format)
    //new BinaryPickleReader(data, mirror, format)

  override def toString = s"""BinaryPickle(${value.mkString("[", ",", "]")})"""
}

case class BinaryPickleStream(input: InputStream) extends BinaryPickle {
  val value: Array[Byte] = Array.ofDim[Byte](0)

  def createReader(mirror: Mirror, format: BinaryPickleFormat): PReader =
    new BinaryPickleReader(new StreamInput(input), mirror, format)

  /* Do not override def toString to avoid traversing the input stream. */
}

case class BinaryInputPickle(input: BinaryInput) extends BinaryPickle {
  val value: Array[Byte] = Array.ofDim[Byte](0)

  def createReader(mirror: Mirror, format: BinaryPickleFormat): PReader =
    new BinaryPickleReader(input, mirror, format)

  /* Do not override def toString to avoid traversing the input stream. */
}

object BinaryPickle {
  //TODO override that stuff
  def apply(a: Array[Byte]): BinaryPickle =
    new BinaryPickleArray(a)
}

class BinaryPickleBuilder(format: BinaryPickleFormat, out: BinaryOutput) extends BinaryPBuilder with PickleTools {
  import format._
  
  private var output: BinaryOutput = out

  @inline private[this] def mkOutput(knownSize: Int): Unit = {
    if (output == null)
      output = if (knownSize != -1) new ByteArrayOutput(knownSize)
               else new ByteArrayOutput
    else
      output.ensureCapacity(knownSize)
  }

  @inline def beginEntry(picklee: Any): PBuilder = withHints { hints =>
    mkOutput(hints.knownSize)

    if (picklee == null) {
      output.putByte( NULL_TAG)
    } else if (hints.oid != -1) {
      output.putByte( REF_TAG)
      output.putInt( hints.oid)
    } else {
      if (!hints.isElidedType) {
        // quickly decide whether we should use picklee.getClass instead
        val ts =
          if (hints.tag.key.contains("anonfun$")) picklee.getClass.getName
          else hints.tag.key
        output.putString( ts)
      }

      // NOTE: it looks like we don't have to write object ids at all
      // traversals employed by pickling and unpickling are exactly the same
      // hence when unpickling it's enough to just increment the nextUnpicklee counter
      // and everything will work out automatically!

      hints.tag.key match { // PERF: should store typestring once in hints.
        case KEY_UNIT =>
          output.putByte(UNIT_TAG)
        case KEY_NULL =>
          output.putByte(NULL_TAG)
        case KEY_BYTE =>
          output.putByte(picklee.asInstanceOf[Byte])
        case KEY_SHORT =>
          output.putShort(picklee.asInstanceOf[Short])
        case KEY_CHAR =>
          output.putChar(picklee.asInstanceOf[Char])
        case KEY_INT =>
          output.putInt(picklee.asInstanceOf[Int])
        case KEY_LONG =>
          output.putLong(picklee.asInstanceOf[Long])
        case KEY_BOOLEAN =>
          output.putBoolean(picklee.asInstanceOf[Boolean])
        case KEY_FLOAT =>
          output.putFloat(picklee.asInstanceOf[Float])
        case KEY_DOUBLE =>
          output.putDouble(picklee.asInstanceOf[Double])
        case KEY_STRING =>
          output.putString(picklee.asInstanceOf[String])
        case KEY_ARRAY_BYTE =>
          output.putByteArray(picklee.asInstanceOf[Array[Byte]])
        case KEY_ARRAY_CHAR =>
          output.putCharArray(picklee.asInstanceOf[Array[Char]])
        case KEY_ARRAY_SHORT =>
          output.putShortArray(picklee.asInstanceOf[Array[Short]])
        case KEY_ARRAY_INT =>
          output.putIntArray(picklee.asInstanceOf[Array[Int]])
        case KEY_ARRAY_LONG =>
          output.putLongArray(picklee.asInstanceOf[Array[Long]])
        case KEY_ARRAY_BOOLEAN =>
          output.putBooleanArray(picklee.asInstanceOf[Array[Boolean]])
        case KEY_ARRAY_FLOAT =>
          output.putFloatArray(picklee.asInstanceOf[Array[Float]])
        case KEY_ARRAY_DOUBLE =>
          output.putDoubleArray(picklee.asInstanceOf[Array[Double]])
        case _ =>
          if (hints.isElidedType) output.putByte(ELIDED_TAG)
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
    output.putInt(length)
    this
  }

  @inline def putElement(pickler: PBuilder => Unit): PBuilder = {
    pickler(this)
    this
  }

  @inline def endCollection(): Unit = {
  }

  @inline def result() = {
    BinaryPickle(output.result)
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

class BinaryPickleReader(in: BinaryInput, mirror: Mirror, format: BinaryPickleFormat) extends AbstractBinaryReader(mirror) with PReader with PickleTools {
  import format._
  
  def beginEntryNoTag(): String =
    beginEntryNoTagDebug(false)

  def beginEntryNoTagDebug(debugOn: Boolean): String = {
    val res: Any = withHints { hints =>
      // if (debugOn)
      //   debug(s"hints: $hints")

      if (hints.isElidedType && nullablePrimitives.contains(hints.tag.key)) {
        val lookahead = in.getByte()
        lookahead match {
          case UNIT_TAG => FastTypeTag.Unit
          case NULL_TAG => FastTypeTag.Null
          case REF_TAG  => FastTypeTag.Ref
          case _        => in.setLookahead(lookahead); hints.tag
        }
      } else if (hints.isElidedType && primitives.contains(hints.tag.key)) {
        hints.tag
      } else {
        val lookahead = in.getByte()
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
              in.getStringWithLookahead(lookahead)
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

  def readPrimitive(): Any = {
    val res = lastTagRead.key match {
      case KEY_NULL    => null
      case KEY_REF     => lookupUnpicklee(in.getInt)
      case KEY_BYTE    => in.getByte
      case KEY_SHORT   => in.getShort
      case KEY_CHAR    => in.getChar
      case KEY_INT     => in.getInt
      case KEY_LONG    => in.getLong
      case KEY_BOOLEAN => in.getBoolean
      case KEY_FLOAT   => in.getFloat
      case KEY_DOUBLE  => in.getDouble

      case KEY_STRING =>  in.getString

      case KEY_ARRAY_BYTE    => in.getByteArray
      case KEY_ARRAY_SHORT   => in.getShortArray
      case KEY_ARRAY_CHAR    => in.getCharArray
      case KEY_ARRAY_INT     => in.getIntArray
      case KEY_ARRAY_LONG    => in.getLongArray
      case KEY_ARRAY_BOOLEAN => in.getBooleanArray
      case KEY_ARRAY_FLOAT   => in.getFloatArray
      case KEY_ARRAY_DOUBLE  => in.getDoubleArray
    }
    res
  }

  def atObject: Boolean = !atPrimitive

  def readField(name: String): BinaryPickleReader =
    this

  def endEntry(): Unit = { /* do nothing */ }

  def beginCollection(): PReader = this

  def readLength(): Int = in.getInt

  def readElement(): PReader = this

  def endCollection(): Unit = { /* do nothing */ }

}

