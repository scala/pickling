package scala.pickling.binary

import scala.pickling._
import scala.pickling.internal._
import scala.language.implicitConversions
import scala.reflect.runtime.universe.Mirror

import java.io.InputStream
import java.nio.ByteBuffer

abstract class BinaryPickle extends Pickle {
  type PickleFormatType = BinaryPickleFormat
  type ValueType = Array[Byte]

  val value: Array[Byte]

  def createReader(format: BinaryPickleFormat): PReader
}

case class BinaryPickleArray(data: Array[Byte]) extends BinaryPickle {
  val value: Array[Byte] = data

  def createReader(format: BinaryPickleFormat): PReader =
    new BinaryPickleReader(new ByteArrayInput(data), format)

  override def toString = s"""BinaryPickle(${value.mkString("[", ",", "]")})"""
}

case class BinaryInputPickle(input: BinaryInput) extends BinaryPickle {
  val value: Array[Byte] = Array.ofDim[Byte](0)

  def createReader(format: BinaryPickleFormat): PReader =
    new BinaryPickleReader(input, format)

  /* Do not override def toString to avoid traversing the input stream. */
}

object BinaryPickle {
  def apply(a: Array[Byte]): BinaryPickle = new BinaryPickleArray(a)
  def apply(a: BinaryInput): BinaryPickle = new BinaryInputPickle(a)
  def apply(a: InputStream): BinaryPickle = new BinaryInputPickle(new StreamInput(a))
  def apply(a: ByteBuffer): BinaryPickle = new BinaryInputPickle(new ByteBufferInput(a))
}

class BinaryPickleBuilder(format: BinaryPickleFormat, out: BinaryOutput) extends BinaryPBuilder with PickleTools {
  import format._
  
  private var output: BinaryOutput = out
  private var isIgnoringFields = false

  @inline private[this] def mkOutput(knownSize: Int): Unit = {
    if (output == null)
      output = if (knownSize != -1) new FixedByteArrayOutput(knownSize)
               else new ByteArrayOutput
    else
      output.ensureCapacity(knownSize)
  }

  private def ignoringSharedRefs(action: => PBuilder): PBuilder =
      if(isIgnoringFields) this
      else action

  @inline def beginEntry(picklee: Any, tag: FastTypeTag[_]): PBuilder = withHints { hints =>
    mkOutput(hints.knownSize)

    if (picklee == null) {
      output.putByte( NULL_TAG)
    } else if (hints.isSharedReference) {
      output.putByte( REF_TAG)
      output.putInt( hints.oid)
      isIgnoringFields = true
    } else {
      if (!hints.isElidedType) {
        // quickly decide whether we should use picklee.getClass instead
        val ts =
          if (tag.key.contains("anonfun$")) picklee.getClass.getName
          else tag.key
        output.putString( ts)
      }

      // NOTE: it looks like we don't have to write object ids at all
      // traversals employed by pickling and unpickling are exactly the same
      // hence when unpickling it's enough to just increment the nextUnpicklee counter
      // and everything will work out automatically!

      tag.key match { // PERF: should store typestring once in hints.
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

  @inline def putField(name: String, pickler: PBuilder => Unit): PBuilder = ignoringSharedRefs {
    // can skip writing name if we pickle/unpickle in the same order
    pickler(this)
    this
  }

  @inline def endEntry(): Unit = {
    /* do nothing */
    // We always reset this:
    isIgnoringFields = false
  }

  @inline def beginCollection(length: Int): PBuilder = ignoringSharedRefs {
    output.putInt(length)
    this
  }

  @inline def putElement(pickler: PBuilder => Unit): PBuilder = ignoringSharedRefs {
    pickler(this)
    this
  }

  @inline def endCollection(): Unit = {
  }

  @inline def result() = {
    BinaryPickle(output.result)
  }

}

abstract class AbstractBinaryReader() {
  protected var _lastTypeStringRead: String  = null
  // TODO - ok to hack this?
  def lastTagRead: String = _lastTypeStringRead
}

class BinaryPickleReader(in: BinaryInput, format: BinaryPickleFormat) extends AbstractBinaryReader() with PReader with PickleTools {
  import format._
  
  def beginEntry: String = {
    val res: Any = withHints { hints =>

      if (hints.isElidedType && nullablePrimitives.contains(hints.elidedType.get.key)) {
        val lookahead = in.getByte()
        lookahead match {
          case UNIT_TAG => FastTypeTag.Unit
          case NULL_TAG => FastTypeTag.Null
          case REF_TAG  => FastTypeTag.Ref
          case _        => in.setLookahead(lookahead); hints.elidedType.get
        }
      } else if (hints.isElidedType && primitives.contains(hints.elidedType.get.key)) {
        hints.elidedType.get
      } else {
        val lookahead = in.getByte()
        lookahead match {
          case NULL_TAG =>
            FastTypeTag.Null
          case ELIDED_TAG =>
            hints.elidedType.getOrElse(throw new PicklingException(s"Type is elided in pickle, but no elide hint was provided by unpickler!"))
          case REF_TAG =>
            FastTypeTag.Ref
          case _ =>
            // do not consume lookahead byte
            val res = try {
              in.getStringWithLookahead(lookahead)
            } catch {
              case PicklingException(msg, cause) =>
                throw PicklingException(s"error decoding type string. debug info: $hints\ncause:$msg")
            }
            res
        }
      }
    }
    if (res.isInstanceOf[String]) {
      _lastTypeStringRead = res.asInstanceOf[String]
      _lastTypeStringRead
    } else {
      _lastTypeStringRead = res.asInstanceOf[FastTypeTag[_]].key
      _lastTypeStringRead
    }
  }

  //def beginEntry(): FastTypeTag[_] = {
  //  beginEntryNoTag()
  //  lastTagRead
  //}

  def atPrimitive: Boolean = primitives.contains(lastTagRead)

  def readPrimitive(): Any = {
    val res = lastTagRead match {
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

