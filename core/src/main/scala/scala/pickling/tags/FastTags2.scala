package scala.pickling
package tags

import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}

/**
 * A "tag" denoting information about a runtime type.
 * This tag is meant to be extermely efficient for simple runtime checks, avoiding a full reflection overhead, while
 * also *allowing* full runtime checks.
 *
 * Notes:
 *
 * 1. Currently the equals method does "stringy" comparison of types.  For a full "conforms" comparison, you'll need to
 *    fully reify the Type (tpe).
 * 2. It is vitally important to this class that an tag created using `apply` matches those created using `makeRaw`.
 *    As such, a few oddities have arisen:
 *    - Existentials are encoded as `scala.Any` type parameters, rather than erased.
 *    - This class does NOT support refinement types, e.g. `Foo with Baz`.  Indeed, this is not a physical class
 *      at runtime, and therefore the need to "tag" something as having this type is limited.
 *
 *
 * @tparam T
 */
trait FastTypeTag[T] extends Equals {
  /** A stringified key that can be used to denote this type.   This key should be unique for types within scala,
    * although the key will *not* determine uniqueness between types loaded on different classloaders.
    *
    * @return  A stringy type key.
    */
  def key: String

  /** Returns true if this a type constructor with no arguments. */
  def isSimpleType: Boolean

  /** Returns the name of the type constructor for this type.  For simple types, `key == typeConstructor`. */
  def typeConstructor: String

  /** A list of type arguments.  */
  def typeArgs: List[FastTypeTag[_]]

  /**
   * @param otherMirror The mirror where we should reconsititute the Type inside.
   * @return  A new Type instance that has reconstructed the full Type.
   */
  def reflectType(otherMirror: ru.Mirror): ru.Type =
    FastTypeTag.reflectType(otherMirror, this)

 /**
   * Tests whether this tag is effectively a primitive type.  Note: We duplicate logic
   * out of regular runtime reflection here to avoid the burden of requiring runtime reflection.
   */
  def isEffectivelyPrimitive: Boolean =
    FastTypeTag.EffectivePrimitiveTags.contains(key)

  override def canEqual(x: Any) = x.isInstanceOf[FastTypeTag[_]]
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


private[pickling] final case class SimpleFastTypeTag[T](
    typeConstructor: String, 
    typeArgs: List[FastTypeTag[_]]) extends FastTypeTag[T] {
  override def isSimpleType = typeArgs.isEmpty
  override val key = 
    if (typeArgs.isEmpty) typeConstructor 
    else s"$typeConstructor[${typeArgs.map(_.key).mkString(",")}]"
}

object FastTypeTag {
  implicit def apply[T]: FastTypeTag[T] = macro Compat.FastTypeTagMacros_impl[T]

  def unapply[T](tag: FastTypeTag[T]): Option[(String, List[FastTypeTag[_]])] =
    Option(tag.typeConstructor -> tag.typeArgs)

  
  def apply[T](key: String): FastTypeTag[T] = {
    val (tag, rem) = parseKey[T](key)
    if (rem.isEmpty) tag
    else sys.error(s"Could not parse FastTypeTag: $key, remaining: $rem, parsed: $tag")  // TODO - legit error
  }

  def apply[T](tcons: String, targs: List[FastTypeTag[_]]): FastTypeTag[T] =
    new SimpleFastTypeTag(tcons, targs)

  // TODO - can we just leave a cache like this around?  It was in 0.10.x + prior, but
  // perhaps there are better mechanisms to solve this cache issue.
  private val typeFromStringCache = scala.collection.concurrent.TrieMap[String, ru.Type]()
  def reflectType[T](mirror: ru.Mirror, tag: FastTypeTag[T]): ru.Type = {
    def calculate: ru.Type = {
      // TODO - cache the results of this?
      val typename = tag.typeConstructor
      def errorMsg = s"""error: cannot find class or module with type name '$typename'
                        |full type string: '${tag.key}'""".stripMargin
      val sym = try {
        if (typename.endsWith(".type")) mirror.staticModule(typename.stripSuffix(".type")).moduleClass
        else mirror.staticClass(typename)
      } catch {
         case _: ScalaReflectionException => sys.error(errorMsg)
         case _: scala.reflect.internal.MissingRequirementError => sys.error(errorMsg)
      }
      val tycon = sym.asType.toTypeConstructor
      import ru._
      import compat._
      appliedType(tycon, tag.typeArgs.map(_.reflectType(mirror)))
    }
    val stpe = tag.key
    if (typeFromStringCache.contains(stpe)) typeFromStringCache(stpe)
    else {
      val result = calculate
      typeFromStringCache(stpe) = result
      result
    }
  }

  // the delimiters in an applied type
  private val delims = List(',', '[', ']')
  private def parseKey[T](key: String): (FastTypeTag[T], String) = {
    // shape of `key`: fqn[at_1, ..., at_n]
    val (typename, rem) = key.span(!delims.contains(_))

    if (rem.isEmpty || rem.startsWith(",") || rem.startsWith("]")) {
      (SimpleFastTypeTag[T](typename, List()), rem)
    } else { // parse type arguments
      var typeArgs  = List[FastTypeTag[_]]()
      var remaining = rem

      while (remaining.startsWith("[") || remaining.startsWith(",")) {
        remaining = remaining.substring(1)
        val (nextAppliedType, rem) = parseKey(remaining)
        typeArgs = typeArgs :+ nextAppliedType
        remaining = rem
      }

      (SimpleFastTypeTag[T](typename, typeArgs), if (remaining.startsWith("]")) remaining.substring(1) else remaining)
    }
  }

  // Note: this method is only used from runtime-only PICKLERS (not unpicklers).   In hybrid mode, this shouldn't be used.
  def makeRaw[T](clazz: Class[T]): FastTypeTag[T] = {
    rawclassToTagMap.getOrElse(clazz, {
      if (clazz == null) FastTypeTag.Null
      else if (clazz.isArray) {
        val elemClass = clazz.getComponentType()
        val elemTag = makeRaw(elemClass)
        mkArrayTag(elemTag).asInstanceOf[FastTypeTag[T]]
      } else {
        val typeArgs = clazz.getTypeParameters
        val clazzName0 = clazz.getName()
        val clazzName =
            if (clazzName0.endsWith("$")) clazzName0.replace("$", ".type")
            else if (clazzName0.contains("anonfun$") || clazzName0.contains("$colon$colon") || clazzName0.endsWith("$sp")) clazzName0
            else clazzName0.replace('$', '.')
        if (typeArgs.isEmpty) SimpleFastTypeTag(clazzName, Nil)
        else {
          // TODO(jsuereth) - for each parameter, check bounds and generate a raw tag based on the bounds
          // For now just always use `Any`
          val argTags = typeArgs map (i => Any)
          FastTypeTag(clazzName, argTags.toList)
        }
      }
   }).asInstanceOf[FastTypeTag[T]]
  }

  // Default tags
  implicit val Any     = FastTypeTag[Any]("scala.Any")
  implicit val Null    = FastTypeTag[Null]("scala.Null")
  implicit val Byte    = FastTypeTag[Byte]("scala.Byte")
  implicit val Short   = FastTypeTag[Short]("scala.Short")
  implicit val Char    = FastTypeTag[Char]("scala.Char")
  implicit val Int     = FastTypeTag[Int]("scala.Int")
  implicit val Long    = FastTypeTag[Long]("scala.Long")
  implicit val Boolean = FastTypeTag[Boolean]("scala.Boolean")
  implicit val Float   = FastTypeTag[Float]("scala.Float")
  implicit val Double  = FastTypeTag[Double]("scala.Double")
  implicit val Unit    = FastTypeTag[Unit]("scala.Unit")
  implicit val String  = FastTypeTag[java.lang.String]("java.lang.String")
  implicit val AnyRef  = FastTypeTag[AnyRef]("scala.AnyRef")

  // Arrays
  private def mkArrayTag[T](el: FastTypeTag[T]): FastTypeTag[Array[T]] =
    SimpleFastTypeTag("scala.Array", el :: Nil)
  implicit val ArrayString: FastTypeTag[Array[String]] = mkArrayTag(String)
  implicit val ArrayByte: FastTypeTag[Array[Byte]] = mkArrayTag(Byte)
  implicit val ArrayShort: FastTypeTag[Array[Short]] = mkArrayTag(Short)
  implicit val ArrayChar: FastTypeTag[Array[Char]] = mkArrayTag(Char)
  implicit val ArrayInt: FastTypeTag[Array[Int]] = mkArrayTag(Int)
  implicit val ArrayLong: FastTypeTag[Array[Long]] = mkArrayTag(Long)
  implicit val ArrayBoolean: FastTypeTag[Array[Boolean]] = mkArrayTag(Boolean)
  implicit val ArrayFloat: FastTypeTag[Array[Float]] = mkArrayTag(Float)
  implicit val ArrayDouble: FastTypeTag[Array[Double]] = mkArrayTag(Double)
  implicit val ArrayUnit: FastTypeTag[Array[Unit]] = mkArrayTag(Unit)
  implicit val ArrayAnyRef: FastTypeTag[Array[AnyRef]] = mkArrayTag(AnyRef)

  implicit val Nothing: FastTypeTag[Nothing] = 
    FastTypeTag[Nothing]("scala.Nothing")
  implicit val Ref = 
    FastTypeTag[refs.Ref]("scala.pickling.refs.Ref")

  // NOTE: should only really be used by makeRaw and "runtime pickler generators".
  private val rawclassToTagMap = Map[Class[_], FastTypeTag[_]](
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

  // NOTE: This is a bit of a hack, copied from [[Symbols.isPrimitive]]
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

  /** The jvm class name mapping of the type tags. */
  // TODO - figure out if this is actually needed anywhere
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
}

trait FastTypeTagMacros extends Macro {
  // TODO(joshuasuereth): This is may duplicate functionality with the `.tag` extension method on `Type`.
  def impl[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val T = weakTypeOf[T]
    if (T.typeSymbol.isParameter)
      c.abort(c.enclosingPosition, s"cannot generate FastTypeTag for type parameter $T, FastTypeTag can only be generated for concrete types")
    // TODO(jsuereth) - make sure `with ...` does not show up in the resulting string.
    def handleType(t: c.Type): c.Tree =
     t.normalize match {
      case ExistentialType(tparams, TypeRef(pre, sym, targs))
	if targs.nonEmpty && targs.forall(targ => tparams.contains(targ.typeSymbol)) =>
          // rather than going down form List[_] => List we want to become List[Any],
          // we are trying to make this match the java-reflection case (where it will be Any)
          handleType(TypeRef(pre, sym, targs.map(_ => definitions.AnyTpe)))
      case TypeRef(pre, sym, targs) if pre.typeSymbol.isModuleClass =>
	val name = sym.fullName + (if (sym.isModuleClass) ".type" else "")
	val targSrcs = targs.map(t => q"_root_.scala.Predef.implicitly[_root_.scala.pickling.tags.FastTypeTag[${t}]]")
	q"_root_.scala.pickling.tags.FastTypeTag[$T]($name, _root_.scala.List.apply(..$targSrcs))"
      // TOOD(jsuereth) - more robust refinement type handling (T with U)
      case _ if T.toString contains "with" =>
         val sub = T.toString.replaceAll(" with .*", "")
         c.warning(c.enclosingPosition, s"cannot generate stable FastTypeTag for refinement type $T, using $sub")
	q"_root_.scala.pickling.tags.FastTypeTag[$T](${sub}, _root_.scala.Nil)"   
      case _ =>
	q"_root_.scala.pickling.tags.FastTypeTag[$T](${T.toString}, _root_.scala.Nil)"
    }
    handleType(T)
  }
}

import scala.language.experimental.macros
import scala.language.existentials

import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}

// this is only necessary because 2.10.x doesn't support macro bundles
object Compat {
  def FastTypeTagMacros_impl[T: c.WeakTypeTag](c: Context): c.Expr[FastTypeTag[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr[FastTypeTag[T]](bundle.impl[T])
  }
}

