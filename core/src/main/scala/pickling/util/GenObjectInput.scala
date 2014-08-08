package scala.pickling.util

import scala.language.experimental.macros

import scala.reflect.macros.Context

import java.io.ObjectInput


object GenObjectInput {

  /**
   *  @tparam T    a tuple type representing a sequence of types read from the ObjectInput
   *  @param  args a tuple with the values with which the ObjectInput should be initialized
   */
  def genInstance[T](arg: T): ObjectInput = macro genInstanceImpl[T]

  // TODO: fresh name for ObjectInput subclass
  def genInstanceImpl[T: c.WeakTypeTag](c: Context)(arg: c.Expr[T]): c.Expr[ObjectInput] = {
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
      else if (state == 3) x3
    }
    */

    def readTree(is: List[Int]): Tree =
      if (is.isEmpty) q"???"
      else if (is.size == 1) {
        val argName = TermName("x" + is.head)
        q"state += 1; $argName"
      } else if (is.size == 2) {
        val argName0 = TermName("x" + is.head)
        val argName1 = TermName("x" + is.tail.head)
        q"""
          state += 1
          if (state == ${is.head}) $argName0
          else $argName1
        """
      } else if (is.size == 3) {
        val argName0 = TermName("x" + is.head)
        val argName1 = TermName("x" + is.tail.head)
        val argName2 = TermName("x" + is.tail.tail.head)
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
      val name = TermName("x" + i)
      q"val $name: $targ"
    }

    // per type a sorted list of indices
    val perType = targs.zipWithIndex.groupBy { case (targ, i) => targ }
                       .map { case (targ, things) => (targ, (things.map { case (_, i) => i }).sorted) }

    def finalTree(tpe: Type): Tree =
      readTree(perType.getOrElse(tpe, List[Int]()))

    val instance: Tree = q"""
      class DummyObjectInput(..$params) extends ObjectInput {
        var state = -1
        def readByte(): Byte = ${finalTree(ByteTpe)}
        def readBoolean(): Boolean = ${finalTree(BooleanTpe)}
        def readChar(): Char = ${finalTree(CharTpe)}
        def readDouble(): Double = ???
        def readFloat(): Float = ???
        def readFully(x1: Array[Byte], x2: Int, x3: Int): Unit = ???
        def readFully(x1: Array[Byte]): Unit = ???
        def readInt(): Int = ???
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
        def readObject(): Object = ???
        def skip(x1: Long): Long = ???
      }
      new DummyObjectInput(..$args)
    """

    c.Expr[ObjectInput](instance)
  }
}
