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

    type PickleType = BinaryPickle

    /* stack of incomplete entry states */
    private var entries = List[EntryState]()
    private var completed: Array[Byte] = _

    private class EntryState(val tpe: Type) {
      var target: Array[Byte] = _
      /* current write index into `target` */
      var pos = 0
    }

    private def formatType(tpe: Type): Array[Byte] = {
      val s = tpe match {
        case TypeRef(_, sym, Nil) => s"${sym.fullName}"
        case TypeRef(_, sym, targs) => ???
      }
      s.getBytes("UTF-8")
    }

    //TODO: pass size hint, so that an array of the right size can be allocated
    def beginEntry(tpe: Type, picklee: Any): this.type = {
      // we always need to push an entry to the stack
      // even for primitive types, since we'll need to check the
      // type upon endEntry
      val entry = new EntryState(tpe)
      // push entry to stack
      entries = entry :: entries

      if (format.isPrimitive(tpe)) {
        val current = entries.tail.head

        // 0. save position of entry length
        val entryLenPos = current.pos
        var entryLenSoFar = 0

        // 1. store length of type name
        current.pos = current.pos + 4
        val tpeBytes = formatType(tpe)
        Util.encodeIntTo(current.target, current.pos, tpeBytes.length)

        // 2. store type name
        Util.copy(current.target, current.pos + 4, tpeBytes)
        current.pos = current.pos + 4 + tpeBytes.length
        entryLenSoFar = 4 + tpeBytes.length

        val sym = tpe.typeSymbol.asClass
        // 3. store primitive in following bytes of current array
        if (sym == IntClass) {
          Util.encodeIntTo(current.target, current.pos, picklee.asInstanceOf[Int])
          entryLenSoFar += 4
          current.pos = current.pos + 4
        } else if (sym == StringClass) {
          val next = Util.encodeStringTo(current.target, current.pos, picklee.asInstanceOf[String])
          entryLenSoFar += (next - current.pos)
          current.pos = current.pos + (next - current.pos)
        } else if (sym == BooleanClass) {
          Util.encodeBooleanTo(current.target, current.pos, picklee.asInstanceOf[Boolean])
          entryLenSoFar += 1
          current.pos = current.pos + 1
        }

        // 4. store entry length at the beginning of this entry
        Util.encodeIntTo(current.target, entryLenPos, entryLenSoFar)
        this
      } else {
        // allocate array (what size?)
        // one approach (slow) would be to allocate a "big enough" array
        // and later when we know the required size, because we're done,
        // copy everything into a smaller array of the right size      
        // this array will later on store length of the entire entry in its first 4 bytes
        val target = Array.ofDim[Byte](4096)
        entry.target = target

        // reserve 4 bytes to store length of entire entry
        val offset = 4

        // write pickled tpe to `target`:
        // length of pickled type, pickled type
        val tpeBytes = formatType(tpe)
        Util.encodeIntTo(target, offset, tpeBytes.length)
        Util.copy(target, offset + 4, tpeBytes)

        // advance current write index
        entry.pos = tpeBytes.length + offset + 4

        this
      }
    }

    def putField(name: String, pickler: this.type => Unit): this.type = {
      // can skip writing name if we pickle/unpickle in the same order
      pickler(this)
      this
    }

    def endEntry(): Unit = {
      val current = entries.head
      // pop current entry from stack
      entries = entries.tail

      if (format.isPrimitive(current.tpe)) () // do nothing
      else {
        // we know the length of the current entry
        // write its length and its contents to the parent entry
        val totalLen = current.pos
        Util.encodeIntTo(current.target, 0, totalLen - 4)

        // save result, in case we're done
        if (entries.isEmpty) {
          // allocate new array to put into completed
          completed = Array.ofDim[Byte](totalLen)
          Array.copy(current.target, 0, completed, 0, totalLen)
        } else {
          // copy to parent array
          val parent = entries.head
          Array.copy(current.target, 0, parent.target, parent.pos, totalLen)
          parent.pos = parent.pos + totalLen
        }
      }
    }

    def result(): PickleType =
      BinaryPickle(completed)
  }

  class BinaryPickleReader(arr: Array[Byte], format: BinaryPickleFormat) extends PickleReader {
    import ru._

    private var pos = 0
    private var atPrim = false

    def readType(mirror: Mirror): Type = {
      def unpickleTpe(stpe: String): Type = {
        // TODO: support polymorphic types as serialized above with pickleTpe
        mirror.staticClass(stpe).asType.toType
      }
      val offset = 4
      val typeString = Util.decodeStringFrom(arr, offset)
      pos = pos + 4 + offset + typeString.length
      val tpe = unpickleTpe(typeString)
      atPrim = format.isPrimitive(tpe)
      tpe
    }

    def atPrimitive: Boolean = atPrim

    def readPrimitive(tpe: Type): Any = {
      if      (tpe =:= typeOf[Int])     Util.decodeIntFrom(arr, pos)._1
      else if (tpe =:= typeOf[String])  Util.decodeStringFrom(arr, pos)
      else if (tpe =:= typeOf[Boolean]) Util.decodeBooleanFrom(arr, pos)._1
      else ???
    }

    def atObject: Boolean = !atPrimitive

    def readField(name: String): BinaryPickleReader = {
      // we don't read field names
      // therefore, just return a reader pointing to the right array

      // at current position we should find the length of the subarray
      // need that length + 4 from current position for the subarray
      val (lenOfEntry, _) = Util.decodeIntFrom(arr, pos)
      val subarrlen = lenOfEntry + 4
      val subarr = Util.readBytesFrom(arr, pos, subarrlen)
      pos = pos + subarrlen
      new BinaryPickleReader(subarr, format)
    }
  }

  class BinaryPickleFormat extends PickleFormat {
    import ru._
    import definitions._

    type PickleType = BinaryPickle
    type PickleBuilderType = BinaryPickleBuilder
    type PickleReaderType = BinaryPickleReader

    def createBuilder(): PickleBuilderType =
      new BinaryPickleBuilder(this)

    def createReader(pickle: PickleType): PickleReaderType =
      new BinaryPickleReader(pickle.value, this)

    def isPrimitive(tpe: Type) = {
      val sym = tpe.typeSymbol.asClass
      sym == IntClass || sym == StringClass || sym == BooleanClass
    }
  }

}

package binary {

  object Util {

    def copy(target: Array[Byte], pos: Int, arr: Array[Byte]): Unit = {
      // def copy(src: Object, srcPos: Int, dest: Object, destPos: Int, length: Int)
      Array.copy(arr, 0, target, pos, arr.length)
    }

    /** Returns decoded Int plus next "readable" position in target array.
     */
    def decodeIntFrom(arr: Array[Byte], i: Int): (Int, Int) = {
      val fst = (arr(i) << 24).toInt
      val snd = ((arr(i+1) << 16) & 0x00FFFFFF).toInt
      val thrd = ((arr(i+2) << 8) & 0x0000FFFF).toInt
      val frth = (arr(i+3) & 0x000000FF).toInt
      (fst | snd | thrd | frth, i+4)
    }

    /** Returns next "writeable" position in target array.
     */
    def encodeIntTo(arr: Array[Byte], i: Int, value: Int): Int = {
      val fst = (value >>> 24).asInstanceOf[Byte]
      val snd = (value >>> 16 & 0xff).asInstanceOf[Byte]
      val thrd = (value >>> 8 & 0xff).asInstanceOf[Byte]
      val frth = (value & 0xff).asInstanceOf[Byte]
      arr(i) = fst
      arr(i+1) = snd
      arr(i+2) = thrd
      arr(i+3) = frth
      i+4
    }

    def decodeBooleanFrom(arr: Array[Byte], i: Int): (Boolean, Int) = {
      val res = arr(i) != 0
      (res, i + 1)
    }

    def encodeBooleanTo(arr: Array[Byte], i: Int, value: Boolean): Int = {
      arr(i) = if (value) 1 else 0
      i + 1
    }

    def readBytesFrom(arr: Array[Byte], i: Int, len: Int): Array[Byte] = {
      val subarr = Array.ofDim[Byte](len)
      Array.copy(arr, i, subarr, 0, len)
      subarr
    }

    def decodeStringFrom(arr: Array[Byte], i: Int): String = {
      val (len, _) = decodeIntFrom(arr, i)
      val bytes = Array.ofDim[Byte](len)
      Array.copy(arr, i + 4, bytes, 0, len)
      new String(bytes, "UTF-8")
    }

    def encodeStringTo(arr: Array[Byte], i: Int, value: String): Int = {
      val bytes = value.getBytes("UTF-8")
      // encode length
      val next = encodeIntTo(arr, i, bytes.length)
      // encode bytes of string at `next` position
      Util.copy(arr, next, bytes)
      next + bytes.length
    }

  }

}
