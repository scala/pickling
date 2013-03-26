package scala.pickling

package object binary {
  implicit val pickleFormat = new BinaryPickleFormat
  implicit def toBinaryPickle(value: Array[Byte]): BinaryPickle = BinaryPickle(value)
}

package binary {
  import scala.reflect.runtime.{universe => ru}
  import scala.reflect.runtime.universe._

  case class BinaryPickle(value: Array[Byte]) extends Pickle {
    type ValueType = Array[Byte]
    type PickleFormatType = BinaryPickleFormat
    override def toString = s"""BinaryPickle(${value.mkString("[", ",", "]")})"""
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

    private val primitives = Map[String, Any => Unit](
      typeTag[Null].key -> ((picklee: Any) => pos = byteBuffer.encodeByteTo(pos, format.NULL_TAG)),
      typeTag[Int].key -> ((picklee: Any) => pos = byteBuffer.encodeIntTo(pos, picklee.asInstanceOf[Int])),
      typeTag[Boolean].key -> ((picklee: Any) => pos = byteBuffer.encodeBooleanTo(pos, picklee.asInstanceOf[Boolean])),
      typeTag[String].key -> ((picklee: Any) => pos = byteBuffer.encodeStringTo(pos, picklee.asInstanceOf[String])),
      typeTag[java.lang.String].key -> ((picklee: Any) => pos = byteBuffer.encodeStringTo(pos, picklee.asInstanceOf[String]))
    )

    def beginEntry(picklee: Any): this.type = withHints { hints =>
      mkByteBuffer(hints.knownSize)

      if (picklee == null) {
        pos = byteBuffer.encodeByteTo(pos, format.NULL_TAG)
      } else if (hints.isElidedType && primitives.contains(hints.tag.key)) {
        primitives(hints.tag.key)(picklee)
      } else {
        if (hints.isElidedType) pos = byteBuffer.encodeByteTo(pos, format.ELIDED_TAG)
        else {
          // write pickled tpe to `target`:
          // length of pickled type, pickled type
          val tpe = hints.tag.tpe
          val tpeBytes = formatType(tpe)
          pos = byteBuffer.encodeIntTo(pos, tpeBytes.length)
          pos = byteBuffer.copyTo(pos, tpeBytes)
          if (primitives.contains(hints.tag.key)) primitives(hints.tag.key)(picklee)
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

    def beginCollection(length: Int): this.type = {
      pos = byteBuffer.encodeIntTo(pos, length)
      this
    }

    def putElement(pickler: this.type => Unit): this.type = {
      pickler(this)
      this
    }

    def endCollection(): Unit = { /* do nothing */ }

    def result() = {
      BinaryPickle(byteBuffer.toArray)
    }
  }

  class BinaryPickleReader(arr: Array[Byte], val mirror: Mirror, format: BinaryPickleFormat) extends PickleReader with PickleTools {
    private val byteBuffer: ByteBuffer = new ByteArray(arr)
    private var pos = 0
    private var lastTagRead: TypeTag[_] = null

    private val primitives = Map[String, () => (Any, Int)](
      typeTag[Null].key -> (() => (null, pos)),
      typeTag[Int].key -> (() => byteBuffer.decodeIntFrom(pos)),
      typeTag[Boolean].key -> (() => byteBuffer.decodeBooleanFrom(pos)),
      typeTag[String].key -> (() => byteBuffer.decodeStringFrom(pos)),
      typeTag[java.lang.String].key -> (() => byteBuffer.decodeStringFrom(pos))
    )

    def beginEntry(): TypeTag[_] = withHints { hints =>
      lastTagRead = {
        if (hints.tag.key == typeTag[String].key || hints.tag.key == typeTag[java.lang.String].key) {
          val (lookahead, newpos) = byteBuffer.decodeByteFrom(pos)
          lookahead match {
            case format.NULL_TAG =>
              pos = newpos
              typeTag[Null]
            case _ =>
              hints.tag
          }
        } else if (hints.isElidedType && primitives.contains(hints.tag.key)) {
          hints.tag
        } else {
          val (lookahead, newpos) = byteBuffer.decodeByteFrom(pos)
          lookahead match {
            case format.NULL_TAG =>
              pos = newpos
              typeTag[Null]
            case format.ELIDED_TAG =>
              pos = newpos
              hints.tag
            case _ =>
              val (typeString, newpos) = byteBuffer.decodeStringFrom(pos)
              pos = newpos
              TypeTag(typeFromString(mirror, typeString), typeString)
          }
        }
      }
      lastTagRead
    }

    def atPrimitive: Boolean = primitives.contains(lastTagRead.key)

    def readPrimitive(): Any = {
      val (res, newpos) = primitives(lastTagRead.key)()
      pos = newpos
      res
    }

    def atObject: Boolean = !atPrimitive

    def readField(name: String): BinaryPickleReader =
      this

    def endEntry(): Unit = { /* do nothing */ }

    def beginCollection(): PickleReader = this

    def readLength(): Int = {
      val (length, newpos) = byteBuffer.decodeIntFrom(pos)
      pos = newpos
      length
    }

    def readElement(): PickleReader = this

    def endCollection(): Unit = { /* do nothing */ }
  }

  class BinaryPickleFormat extends PickleFormat {
    val ELIDED_TAG: Byte = -1
    val NULL_TAG: Byte = -2

    type PickleType = BinaryPickle
    def createBuilder() = new BinaryPickleBuilder(this)
    def createReader(pickle: PickleType, mirror: Mirror) = new BinaryPickleReader(pickle.value, mirror, this)
  }
}
