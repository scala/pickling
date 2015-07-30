package scala.pickling.util

import scala.language.experimental.macros

import scala.reflect.macros.Context
import java.io.ObjectInput


object Externalizables {

  /** Generates a specialized `ObjectInput` instance.
   *
   *  @tparam T    a tuple type representing a sequence of types read from the ObjectInput.
   *  @param  args a tuple with the values with which the ObjectInput should be initialized.
   */
  def genInput[T](arg: T): ObjectInput = macro genInputImpl[T]

  /** Generates a specialized `ArrayObjectOutput` instance.
   *
   *  Note that we do not need a whitebox macro, since the return type does not need
   *  refinements, thanks to using arrays to store multiple values of the same type.
   *
   *  @tparam T  a tuple type representing a sequence of types written to the ArrayObjectOutput.
   */
  def genOutput[T]: ArrayObjectOutput = macro genOutputImpl[T]

  def genInputImpl[T: c.WeakTypeTag](c: Context)(arg: c.Expr[T]): c.Expr[ObjectInput] = {
    import c.universe._
    import definitions.{BooleanTpe, ByteTpe, CharTpe, DoubleTpe, FloatTpe, IntTpe, LongTpe, ShortTpe}

    val tag = weakTypeTag[T]

    // TODO: abort if T not a tuple type

    val targs = tag.tpe match {
      case TypeRef(_, _, args) => args
    }

    // extract argument trees of `arg` tuple (to avoid allocating and destructuring a tuple)
    // note: passing a tuple created using `->` doesn't work
    val q"$path[..$ts](..$args)" = arg.tree

    /* Int, Int, Array[Byte], Int, Long, Object
         0,   1,           2,   3,    4,      5

       def readInt(): Int = {
         state += 1
         if (state == 0) x0
         else if (state == 1) x1
         else x3
       }
    */

    def readTree(is: List[Int]): Tree =
      if (is.isEmpty) q"???"
      else if (is.size == 1) {
        val argName = newTermName("x" + is.head)
        q"state += 1; $argName"
      } else if (is.size == 2) {
        val argName0 = newTermName("x" + is.head)
        val argName1 = newTermName("x" + is.tail.head)
        q"""
          state += 1
          if (state == ${is.head}) $argName0
          else $argName1
        """
      } else if (is.size == 3) {
        val argName0 = newTermName("x" + is.head)
        val argName1 = newTermName("x" + is.tail.head)
        val argName2 = newTermName("x" + is.tail.tail.head)
        q"""
          state += 1
          if (state == ${is.head}) $argName0
          else if (state == ${is.tail.head}) $argName1
          else $argName2
        """
      } else
        c.abort(c.enclosingPosition, "more arguments not supported")

    // create class parameter list
    // (val x0: Int, val x1: Int, val x2: Array[Byte], val x3: Int, val x4: Long, ...)
    val params = for ((targ, i) <- targs.zipWithIndex) yield {
      val name = newTermName("x" + i)
      q"val $name: $targ"
    }

    // per type a sorted list of indices
    val perType = targs.zipWithIndex.groupBy { case (targ, i) => targ }
                       .map { case (targ, things) => (targ, (things.map { case (_, i) => i }).sorted) }

    def finalTree(tpe: Type): Tree =
      readTree(perType.getOrElse(tpe, List[Int]()))

    // copy contents of `xi` where i is the index of `typeOf[Array[Byte]]` in `perType` map
    val readFullyTree: Tree = {
      val arrIndices = perType.getOrElse(typeOf[Array[Byte]], List[Int]())
      if (arrIndices.isEmpty) { // Array[Byte] unused
        q"???"
      } else {
        val name = newTermName("x" + arrIndices.head)
        // TODO: faster array copy using Unsafe?
        q"System.arraycopy($name, 0, x, 0, x.length)"
      }
    }

    val dummyName = c.fresh(newTypeName("DummyObjectInput"))
    val instance: Tree = q"""
      class $dummyName(..$params) extends java.io.ObjectInput {
        var state = -1
        def readByte(): Byte = ${finalTree(ByteTpe)}
        def readBoolean(): Boolean = ${finalTree(BooleanTpe)}
        def readChar(): Char = ${finalTree(CharTpe)}
        def readDouble(): Double = ???
        def readFloat(): Float = ???
        def readFully(x1: Array[Byte], x2: Int, x3: Int): Unit = ???
        def readFully(x: Array[Byte]): Unit = $readFullyTree
        def readInt(): Int = ${finalTree(IntTpe)}
        def readLine(): String = ???
        def readLong(): Long = ???
        def readShort(): Short = ???
        def readUTF(): String = ???
        def readUnsignedByte(): Int = ???
        def readUnsignedShort(): Int = ???
        def skipBytes(x1: Int): Int = ???
        def available(): Int = ???
        def close(): Unit = ???
        def read(x1: Array[Byte], x2: Int, x3: Int): Int = ???
        def read(x1: Array[Byte]): Int = ???
        def read(): Int = ???
        def readObject(): Object = ${finalTree(typeOf[AnyRef])}
        def skip(x1: Long): Long = ???
      }
      new $dummyName(..$args)
    """

    c.Expr[ObjectInput](instance)
  }

  def genOutputImpl[T: c.WeakTypeTag](c: Context): c.Expr[ArrayObjectOutput] = {
    import c.universe._
    import definitions.{BooleanTpe, ByteTpe, CharTpe, DoubleTpe, FloatTpe, IntTpe, LongTpe, ShortTpe}

    val tag = weakTypeTag[T]

    // TODO: abort if T not a tuple type

    val targs = tag.tpe match {
      case TypeRef(_, _, args) => args
    }

    /* see java.io.DataOutput:
       writeBoolean(boolean v)
       writeByte(int v)
       writeChar(int v)
       ...
    */
    val storage = Map(BooleanTpe -> BooleanTpe, ByteTpe -> IntTpe, CharTpe -> IntTpe, DoubleTpe -> DoubleTpe,
                      FloatTpe -> FloatTpe, IntTpe -> IntTpe, LongTpe -> LongTpe, ShortTpe -> IntTpe,
                      typeOf[AnyRef] -> typeOf[Any])

    /* Byte, Byte, Array[Byte], Byte, Long, Object
          0,    1,           2,    3,    4,      5

       val byteArr: Array[Int] = _  // <tpe>Arr: Array[<storage(tpe)>]
       val longArr: Array[Long] = _
       val arrByteArr: Array[Array[Byte]] = _

       def writeByte(x: Int): Unit = {
         state += 1
         if (state == 0) bytes(0) = x
         else if (state == 1) bytes(1) = x
         else bytes(2) = x // note that the index is 2, not 3, since the array should not be bigger than necessary
       }
    */

    // we assume the methods corresponding to the "writeTree" bodies are called in the right order
    def writeTree(is: List[Int], tpeName: String): Tree = {
      val fldName = newTermName(tpeName + "Arr")
      if (is.isEmpty) q"???"
      else if (is.size == 1) {
        q"{ state += 1; $fldName(0) = x }"
      } else if (is.size == 2) {
        q"""
          state += 1
          if (state == ${is.head}) $fldName(0) = x
          else $fldName(1) = x
        """
      } else if (is.size == 3) {
        q"""
          state += 1
          if (state == ${is.head}) $fldName(0) = x
          else if (state == ${is.tail.head}) $fldName(1) = x
          else $fldName(2) = x
        """
      } else
        c.abort(c.enclosingPosition, "more arguments not supported")
    }

    // per type a sorted list of indices
    val perType = targs.zipWithIndex
                       .groupBy { case (targ, i) => targ }
                       .map { case (targ, things) => (targ, (things.map { case (_, i) => i }).sorted) }

    // create array-valued fields
    // val byteArr: Array[Int] = Array.ofDim[Int](3)
    // ...
    val fields = (for (targ <- storage.keys) yield {
      val TypeRef(_, classSym, _) = targ
      val tpestr     = classSym.name.toString.toLowerCase
      val name       = newTermName(tpestr + "Arr")
      val storageTpe = storage(targ)
      val size       = perType.getOrElse(targ, List[Int]()).size
      q"val $name: Array[$storageTpe] = Array.ofDim[$storageTpe]($size)"
    }) ++ Seq(
      // implementation restriction: only store a single array
      q"val arrByteArr: Array[Array[Byte]] = Array.ofDim[Array[Byte]](1)", {
        val storageTpe = storage(typeOf[AnyRef])
        q"val anyRefArr: Array[$storageTpe] = Array.ofDim[$storageTpe](${perType.getOrElse(typeOf[AnyRef], List[Int]()).size})"
      }
    )

    def finalTree(tpe: Type, tpeName: String): Tree =
      writeTree(perType.getOrElse(tpe, List[Int]()), tpeName)

    val dummyName = c.fresh(newTypeName("DummyObjectOutput"))
    val instance: Tree = q"""
      class $dummyName extends scala.pickling.util.ArrayObjectOutput {
        var state = -1
        ..$fields

        // Members declared in java.io.DataOutput
        def writeBoolean(x: Boolean): Unit = ${finalTree(BooleanTpe, "boolean")}
        def writeByte(x: Int): Unit = ${finalTree(ByteTpe, "byte")}
        def writeBytes(x: String): Unit = ???
        def writeChar(x: Int): Unit = ${finalTree(CharTpe, "char")}
        def writeChars(x: String): Unit = ???
        def writeDouble(x: Double): Unit = ???
        def writeFloat(x: Float): Unit = ???
        def writeInt(x: Int): Unit = ${finalTree(IntTpe, "int")}
        def writeLong(x: Long): Unit = ???
        def writeShort(x: Int): Unit = ???
        def writeUTF(x: String): Unit = ???

        // Members declared in java.io.ObjectOutput
        def close(): Unit = ???
        def flush(): Unit = ???
        def write(x1: Array[Byte],x2: Int,x3: Int): Unit = ???
        def write(x: Array[Byte]): Unit = ${finalTree(typeOf[Array[Byte]], "arrByte")}
        def write(x: Int): Unit = ???
        def writeObject(x: Any): Unit = ${finalTree(typeOf[AnyRef], "anyRef")}
      }
      new $dummyName
    """

    c.Expr[ArrayObjectOutput](instance)
  }

}
