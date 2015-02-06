package scala.pickling.binary

import scala.pickling._
import scala.pickling.internal._
import scala.language.implicitConversions
import scala.reflect.runtime.universe.Mirror

trait BinaryFormats {
  implicit val pickleFormat = new BinaryPickleFormat
  implicit def toBinaryPickle(value: Array[Byte]): BinaryPickle = BinaryPickle(value)
  implicit def binaryPickleToUnpickleOps(value: Array[Byte]): UnpickleOps = new UnpickleOps(BinaryPickle(value))
}

trait BinaryPBuilder extends PBuilder {
  def result(): BinaryPickle
}

class BinaryPickleFormat extends PickleFormat with Constants {
  type PickleType = BinaryPickle
  type OutputType = BinaryOutput
  def createBuilder(): BinaryPBuilder = new BinaryPickleBuilder(this, null)
  def createBuilder(out: BinaryOutput): BinaryPBuilder = new BinaryPickleBuilder(this, out)
  def createBuilder(out: java.nio.ByteBuffer): BinaryPBuilder = createBuilder(new ByteBufferOutput(out))
  def createBuilder(out: java.io.OutputStream): BinaryPBuilder = createBuilder(new StreamOutput(out))
  def createReader(pickle: PickleType) = pickle.createReader(this)
}

trait Constants {
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
}
