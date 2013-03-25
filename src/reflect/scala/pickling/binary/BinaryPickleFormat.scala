package scala.pickling

package object binary {
  implicit val pickleFormat = new BinaryPickleFormat
  implicit def toBinaryPickle(value: Array[Byte]): BinaryPickle = BinaryPickle(value)
}

package binary {
  import scala.reflect.runtime.{universe => ru}
  import scala.reflect.synthetic._
  import scala.reflect.runtime.universe._

  case class BinaryPickle(value: Array[Byte]) extends Pickle {
    type ValueType = Array[Byte]
    type PickleFormatType = BinaryPickleFormat
  }

  class BinaryPickleBuilder(format: BinaryPickleFormat) extends PickleBuilder with PickleTools {
    private var byteBuffer: ByteBuffer = _
    private var pos = 0

    private def formatType(tpe: Type): Array[Byte] = {
      typeToString(tpe).getBytes("UTF-8")
    }

    private def mkByteBuffer(knownSize: Int) = {
      if (byteBuffer == null) {
        byteBuffer =
          if (knownSize != -1) new ByteArray(knownSize)
          else new ByteArrayBuffer
      }
    }

    private val primitives = Map[TypeTag[_], Any => Unit](
      ReifiedNull.tag -> ((picklee: Any) => pos = byteBuffer.encodeByteTo(pos, format.NULL_TAG)),
      ReifiedInt.tag -> ((picklee: Any) => pos = byteBuffer.encodeIntTo(pos, picklee.asInstanceOf[Int])),
      ReifiedBoolean.tag -> ((picklee: Any) => pos = byteBuffer.encodeBooleanTo(pos, picklee.asInstanceOf[Boolean])),
      ReifiedString.tag -> ((picklee: Any) => pos = byteBuffer.encodeStringTo(pos, picklee.asInstanceOf[String]))
    )

    def beginEntry(picklee: Any): this.type = withHints { hints =>
      mkByteBuffer(hints.knownSize)

      if (picklee == null) {
        pos = byteBuffer.encodeByteTo(pos, format.NULL_TAG)
      } else if (primitives.contains(hints.tag)) {
        assert(hints.isElidedType)
        primitives(hints.tag)(picklee)
      } else {
        if (hints.isElidedType) pos = byteBuffer.encodeByteTo(pos, format.ELIDED_TAG)
        else {
          // write pickled tpe to `target`:
          // length of pickled type, pickled type
          val tpe = hints.tag.tpe
          val tpeBytes = formatType(tpe)
          pos = byteBuffer.encodeIntTo(pos, tpeBytes.length)
          pos = byteBuffer.copyTo(pos, tpeBytes)
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

    def result() = {
      BinaryPickle(byteBuffer.toArray)
    }
  }

  class BinaryPickleReader(arr: Array[Byte], val mirror: Mirror, format: BinaryPickleFormat) extends PickleReader with PickleTools {
    private val byteBuffer: ByteBuffer = new ByteArray(arr)
    private var pos = 0
    private var lastTagRead: TypeTag[_] = null

    private val primitives = Map[TypeTag[_], () => (Any, Int)](
      ReifiedNull.tag -> (() => (null, pos)),
      ReifiedInt.tag -> (() => byteBuffer.decodeIntFrom(pos)),
      ReifiedBoolean.tag -> (() => byteBuffer.decodeBooleanFrom(pos)),
      ReifiedString.tag -> (() => byteBuffer.decodeStringFrom(pos))
    )

    def beginEntry(): TypeTag[_] = withHints { hints =>
      lastTagRead = {
        if (hints.tag == ReifiedString.tag) {
          val (lookahead, newpos) = byteBuffer.decodeByteFrom(pos)
          lookahead match {
            case format.NULL_TAG =>
              pos = newpos
              ReifiedNull.tag
            case _ =>
              hints.tag
          }
        } else if (primitives.contains(hints.tag)) {
          assert(hints.isElidedType)
          hints.tag
        } else {
          val (lookahead, newpos) = byteBuffer.decodeByteFrom(pos)
          lookahead match {
            case format.NULL_TAG =>
              pos = newpos
              ReifiedNull.tag
            case format.ELIDED_TAG =>
              pos = newpos
              hints.tag
            case _ =>
              val (typeString, newpos) = byteBuffer.decodeStringFrom(pos)
              pos = newpos
              TypeTag(typeFromString(mirror, typeString))
          }
        }
      }
      lastTagRead
    }

    def atPrimitive: Boolean = primitives.contains(lastTagRead)

    def readPrimitive(): Any = {
      val (res, newpos) = primitives(lastTagRead)()
      pos = newpos
      res
    }

    def atObject: Boolean = !atPrimitive

    def readField(name: String): BinaryPickleReader =
      this

    def endEntry(): Unit = { /* do nothing */ }
  }

  class BinaryPickleFormat extends PickleFormat {
    val ELIDED_TAG: Byte = -1
    val NULL_TAG: Byte = -2

    type PickleType = BinaryPickle
    def createBuilder() = new BinaryPickleBuilder(this)
    def createReader(pickle: PickleType, mirror: Mirror) = new BinaryPickleReader(pickle.value, mirror, this)
  }
}
