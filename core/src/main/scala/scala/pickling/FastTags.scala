package scala.pickling

import scala.language.experimental.macros

import scala.pickling.internal._
import scala.reflect.api.Mirror

import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag

/**
 * A "tag" denoting information about a runtime type.
 * This tag is meant to be extermely efficient for simple runtime checks, avoiding a full reflection overhead, while
 * also *allowing* full runtime checks.
 *
 * Notes:
 *
 * 1. Currently the equals method does "stringy" comparison of types.  For a full equality comparison, you'll need to
 *    fully reify the Type (tpe).
 * 2. Calling `mirror` or `tpe` may cause runtime reflection to be used.
 *
 *
 * @tparam T
 */
trait FastTypeTag[T] extends Equals {
  /** @return The mirror known to be in use when this FastTypeTag was created.
    *
    * It is not guaranteed to be the correct mirror to reify the type against.
    */
  def mirror: ru.Mirror

  /**
   * @return The full Type of T.   This method may need to use runtime reflection to reconstruct the full type.
   */
  def tpe: ru.Type

  /** A stringified key that can be used to denote this type.   This key should be unique for types within scala,
    * although the key will *not* determine uniqueness between types loaded on different classloaders.
    *
    * @return  A stringy type key.
    */
  def key: String

  /**
   * @param otherMirror The mirror where we should reconsititute the Type inside.
   * @return  A new Type instance that has reconstructed the full Type.
   */
  def reflectType(otherMirror: ru.Mirror): ru.Type = typeFromString(otherMirror, key)

  /**
   * Tests whether this tag is effectively a primitive type.  Note: We duplicate logic
   * out of regular runtime reflection here to avoid the burden of requiring runtime reflection.
   */
  def isEffectivelyPrimitive: Boolean =
    FastTypeTag.EffectivePrimitiveTags.contains(key)

  override def canEqual(x: Any) = x.isInstanceOf[FastTypeTag[_]]
  // equals skips runtime reflection because it's potentially
  // expensive and unthreadsafe to force the lazy Type field, and
  // since we typeFromString(key) to get the Type anyhow there's
  // no downside to just using the string (the string has to
  // contain all the information).
  override def equals(x: Any) = canEqual(x) && {
    x match {
      case null => false
      case other: FastTypeTag[_] => this.key == other.key
      case _ => false
    }
  }
  override def hashCode = key.hashCode
  override def toString = "FastTypeTag[" + key + "]"
}

object FastTypeTag {
  implicit def materializeFastTypeTag[T]: FastTypeTag[T] = macro Compat.FastTypeTagMacros_impl[T]

  implicit def materializeFastTypeTagOfClassTag[T]: FastTypeTag[ClassTag[T]] = macro Compat.FastTypeTagMacros_implClassTag[T]

  private def stdTag[T: ru.TypeTag]: FastTypeTag[T] = apply(scala.reflect.runtime.currentMirror, ru.typeOf[T], ru.typeOf[T].key).asInstanceOf[FastTypeTag[T]]

  implicit val Null    = stdTag[Null]
  implicit val Byte    = stdTag[Byte]
  implicit val Short   = stdTag[Short]
  implicit val Char    = stdTag[Char]
  implicit val Int     = stdTag[Int]
  implicit val Long    = stdTag[Long]
  implicit val Boolean = stdTag[Boolean]
  implicit val Float   = stdTag[Float]
  implicit val Double  = stdTag[Double]
  implicit val Unit    = stdTag[Unit]

  implicit val String = stdTag[java.lang.String]

  implicit val ArrayString = stdTag[Array[String]]
  implicit val ArrayByte = stdTag[Array[Byte]]
  implicit val ArrayShort = stdTag[Array[Short]]
  implicit val ArrayChar = stdTag[Array[Char]]
  implicit val ArrayInt = stdTag[Array[Int]]
  implicit val ArrayLong = stdTag[Array[Long]]
  implicit val ArrayBoolean = stdTag[Array[Boolean]]
  implicit val ArrayFloat = stdTag[Array[Float]]
  implicit val ArrayDouble = stdTag[Array[Double]]
  implicit val ArrayUnit = stdTag[Array[Unit]]

  implicit val ArrayAnyRef: FastTypeTag[Array[AnyRef]] = {
    val mirror = scala.reflect.runtime.currentMirror
    val tpe = ru.typeOf[Array[AnyRef]]
    val key = "scala.Array[scala.AnyRef]"
    apply(mirror, tpe, key).asInstanceOf[FastTypeTag[Array[AnyRef]]]
  }

  implicit val Nothing: FastTypeTag[Nothing] = stdTag[Nothing]

  implicit val Ref = stdTag[refs.Ref]

  // NOTE; This is a bit of a hack, copied from [[Symbols.isPrimitive]]
  private val EffectivePrimitiveTags: Set[String] = {
    val primitives = Seq(
      Double, Float, Long, Int, Char, Short, Byte, Unit, Boolean
    )
    // TODO - create array primitives out of the above seq
    val arrayPrimitives = Seq(
      ArrayDouble, ArrayFloat, ArrayLong, ArrayInt, ArrayChar, ArrayShort, ArrayByte, ArrayUnit, ArrayBoolean
    )
    (primitives ++ arrayPrimitives).map(_.key).toSet
  }

  /** Construct a new FastTypeTag where all members are known. */
  def apply(mirror0: ru.Mirror, tpe0: ru.Type, key0: String): FastTypeTag[_] = {
    new FastTypeTag[Nothing] {
      def mirror = mirror0
      def tpe = tpe0
      def key = key0
    }
  }
  /** Construct a new fast type tag that will lazily instantiate the Type. */
  def apply(mirror0: ru.Mirror, key0: String): FastTypeTag[_] =
    new FastTypeTag[Nothing] {
      val mirror = mirror0
      val key = key0
      lazy val tpe = typeFromString(mirror, key)
    }
  /** Construct  anew fast type tage using the currently active pickling Mirror and lazily instantiate the Type. */
  def apply(key: String): FastTypeTag[_] = macro Compat.FastTypeTagMacros_apply

  def apply[T: ru.TypeTag]: FastTypeTag[T] = {
    val ruTpe = implicitly[ru.TypeTag[T]].tpe
    new FastTypeTag[T] {
      def mirror = scala.reflect.runtime.currentMirror
      lazy val tpe = ruTpe
      def key = ruTpe.key
    }
  }

  def valueTypeName(tag: FastTypeTag[_]): String = {
    val clazz: Class[_] = tag match {
      case FastTypeTag.String => classOf[java.lang.String]
      case FastTypeTag.Byte => classOf[java.lang.Byte]
      case FastTypeTag.Short => classOf[java.lang.Short]
      case FastTypeTag.Char => classOf[java.lang.Character]
      case FastTypeTag.Int => classOf[java.lang.Integer]
      case FastTypeTag.Long => classOf[java.lang.Long]
      case FastTypeTag.Boolean => classOf[java.lang.Boolean]
      case FastTypeTag.Float => classOf[java.lang.Float]
      case FastTypeTag.Double => classOf[java.lang.Double]
      case _ => null
    }
    if (clazz == null) tag match {
      case FastTypeTag.Null => "null"
      case FastTypeTag.ArrayString => "[Ljava.lang.String;"
      case FastTypeTag.ArrayInt => "[I"
      case FastTypeTag.ArrayDouble => "[D"
      case FastTypeTag.ArrayBoolean => "[Z"
      case FastTypeTag.ArrayLong => "[J"
      case FastTypeTag.ArrayByte => "[B"
      case FastTypeTag.ArrayFloat => "[F"
      case FastTypeTag.ArrayChar => "[C"
      case FastTypeTag.ArrayShort => "[S"
      case _ => tag.key
    } else clazz.getName
  }

  val raw = Map[Class[_], FastTypeTag[_]](
    classOf[java.lang.String] -> FastTypeTag.String,
    classOf[java.lang.Byte] -> FastTypeTag.Byte,
    classOf[java.lang.Short] -> FastTypeTag.Short,
    classOf[java.lang.Character] -> FastTypeTag.Char,
    classOf[java.lang.Integer] -> FastTypeTag.Int,
    classOf[java.lang.Long] -> FastTypeTag.Long,
    classOf[java.lang.Boolean] -> FastTypeTag.Boolean,
    classOf[java.lang.Float] -> FastTypeTag.Float,
    classOf[java.lang.Double] -> FastTypeTag.Double,

    classOf[Byte] -> FastTypeTag.Byte,
    classOf[Short] -> FastTypeTag.Short,
    classOf[Char] -> FastTypeTag.Char,
    classOf[Int] -> FastTypeTag.Int,
    classOf[Long] -> FastTypeTag.Long,
    classOf[Boolean] -> FastTypeTag.Boolean,
    classOf[Float] -> FastTypeTag.Float,
    classOf[Double] -> FastTypeTag.Double,

    classOf[Array[String]] -> FastTypeTag.ArrayString,
    classOf[Array[Int]] -> FastTypeTag.ArrayInt,
    classOf[Array[Byte]] -> FastTypeTag.ArrayByte,
    classOf[Array[Short]] -> FastTypeTag.ArrayShort,
    classOf[Array[Char]] -> FastTypeTag.ArrayChar,
    classOf[Array[Long]] -> FastTypeTag.ArrayLong,
    classOf[Array[Boolean]] -> FastTypeTag.ArrayBoolean,
    classOf[Array[Float]] -> FastTypeTag.ArrayFloat,
    classOf[Array[Double]] -> FastTypeTag.ArrayDouble
  )

  def mkRawArrayTypeAndKey(clazz: Class[_], mirror: ru.Mirror): (ru.Type, String) = {
    // create Type without going through `typeFromString`
    val elemClass = clazz.getComponentType()
    // debug(s"creating tag for array with element type '${elemClass.getName}'")

    val (elemTpe, elemKey) = if (elemClass.isArray) {
      mkRawArrayTypeAndKey(elemClass, mirror)
    } else {
      val elemClassSymbol = try {
        mirror.classSymbol(elemClass)
      } catch {
        case t: Throwable =>
          sys.error(s"error: could not find class '${elemClass.getName}' in runtime mirror")
      }
      val primitiveTag: FastTypeTag[_] = raw.getOrElse(elemClass, null)
      val k = if (primitiveTag == null) elemClass.getName else primitiveTag.key
      (elemClassSymbol.asType.toType, k)
    }

    val tpe = ru.appliedType(ru.definitions.ArrayClass.toType, List(elemTpe))
    val key = "scala.Array[" + elemKey + "]"
    (tpe, key)
  }

  def mkRawArray(clazz: Class[_], mirror: ru.Mirror): FastTypeTag[_] = {
    val (tpe, key) = mkRawArrayTypeAndKey(clazz, mirror)
    apply(mirror, tpe, key)
  }

  def mkRaw(clazz: Class[_], mirror: ru.Mirror): FastTypeTag[_] =
    if (clazz == null) FastTypeTag.Null
    else try {
      raw.getOrElse(clazz, {
        // debug(s"!!! could not find primitive tag for class ${clazz.getName} !!!")
        // handle arrays of non-primitive element type
        if (clazz.isArray) mkRawArray(clazz, mirror)
        else {
          val clazzName0 = clazz.getName()
          val clazzName =
            if (clazzName0.contains("anonfun$") || clazzName0.contains("$colon$colon") || clazzName0.endsWith("$") || clazzName0.endsWith("$sp")) clazzName0
            else clazzName0.replace('$', '.')
          apply(mirror, clazzName)
        }
      })
    } catch {
      case t: Throwable =>
        sys.error(s"error: could not create FastTypeTag for class '${clazz.getName}'")
    }
}

trait FastTypeTagMacros extends Macro {
  def impl[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val T = weakTypeOf[T]
    if (T.typeSymbol.isParameter)
      c.abort(c.enclosingPosition, s"cannot generate FastTypeTag for type parameter $T, FastTypeTag can only be generated for concrete types")

    q"""
      new _root_.scala.pickling.FastTypeTag[$T] {
        def mirror = _root_.scala.pickling.internal.`package`.currentMirror
        lazy val tpe = _root_.scala.reflect.runtime.universe.weakTypeOf[$T]
        def key = ${T.key}
      }
    """
  }
  def implClassTag[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val T = weakTypeOf[T]
    q"""
      new _root_.scala.pickling.FastTypeTag[ClassTag[$T]] {
        def mirror = _root_.scala.pickling.internal.`package`.currentMirror
        lazy val tpe = _root_.scala.reflect.runtime.universe.weakTypeOf[ClassTag[$T]]
        def key = "ClassTag[" + ${T.key} + "]"
      }
    """
  }
  def apply(key: c.Tree): c.Tree = {
    import c.universe._
    q"""_root_.scala.pickling.FastTypeTag(_root_.scala.pickling.internal.`package`.currentMirror, $key)"""
  }
}
