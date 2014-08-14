package scala.pickling

import scala.pickling.internal._
import scala.reflect.macros.Context
import scala.reflect.api.{Universe => ApiUniverse}
import scala.reflect.runtime.{universe => ru}
import language.experimental.macros
import scala.reflect.ClassTag

trait FastTypeTag[+T] extends Equals {
  def mirror: ru.Mirror
  def tpe: ru.Type
  def key: String
  override def canEqual(x: Any) = x.isInstanceOf[FastTypeTag[_]]
  override def equals(x: Any) = x.isInstanceOf[FastTypeTag[_]] && this.mirror == x.asInstanceOf[FastTypeTag[_]].mirror && this.tpe == x.asInstanceOf[FastTypeTag[_]].tpe
  override def hashCode = mirror.hashCode * 31 + tpe.hashCode
  override def toString = "FastTypeTag[" + tpe + "]"
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

  val ScalaString = stdTag[String]
  implicit val JavaString = stdTag[java.lang.String]

  implicit val ArrayByte = stdTag[Array[Byte]]
  implicit val ArrayShort = stdTag[Array[Short]]
  implicit val ArrayChar = stdTag[Array[Char]]
  implicit val ArrayInt = stdTag[Array[Int]]
  implicit val ArrayLong = stdTag[Array[Long]]
  implicit val ArrayBoolean = stdTag[Array[Boolean]]
  implicit val ArrayFloat = stdTag[Array[Float]]
  implicit val ArrayDouble = stdTag[Array[Double]]
  implicit val Nothing: FastTypeTag[Nothing] = stdTag[Nothing]

  implicit val Ref = stdTag[refs.Ref]

  def apply(mirror0: ru.Mirror, tpe0: ru.Type, key0: String): FastTypeTag[_] = {
    new FastTypeTag[Nothing] {
      def mirror = mirror0
      def tpe = tpe0
      def key = key0
    }
  }

  def apply(mirror: ru.Mirror, key: String): FastTypeTag[_] = apply(mirror, typeFromString(mirror, key), key)
  def apply(key: String): FastTypeTag[_] = macro Compat.FastTypeTagMacros_apply

  def valueTypeName(tag: FastTypeTag[_]): String = {
    val clazz: Class[_] = tag match {
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
    if (clazz == null) tag.key else clazz.getName
  }

  val raw = Map[Class[_], FastTypeTag[_]](
    classOf[java.lang.Byte] -> FastTypeTag.Byte,
    classOf[java.lang.Short] -> FastTypeTag.Short,
    classOf[java.lang.Character] -> FastTypeTag.Char,
    classOf[java.lang.Integer] -> FastTypeTag.Int,
    classOf[java.lang.Long] -> FastTypeTag.Long,
    classOf[java.lang.Boolean] -> FastTypeTag.Boolean,
    classOf[java.lang.Float] -> FastTypeTag.Float,
    classOf[java.lang.Double] -> FastTypeTag.Double,
    classOf[Array[Int]] -> FastTypeTag.ArrayInt,
    classOf[Array[Byte]] -> FastTypeTag.ArrayByte,
    classOf[Array[Short]] -> FastTypeTag.ArrayShort,
    classOf[Array[Char]] -> FastTypeTag.ArrayChar,
    classOf[Array[Long]] -> FastTypeTag.ArrayLong,
    classOf[Array[Boolean]] -> FastTypeTag.ArrayBoolean,
    classOf[Array[Float]] -> FastTypeTag.ArrayFloat,
    classOf[Array[Double]] -> FastTypeTag.ArrayDouble
  )

  def mkRaw(clazz: Class[_], mirror: ru.Mirror): FastTypeTag[_] =
    if (clazz == null) FastTypeTag.Null
    else raw.getOrElse(clazz, { apply(mirror, clazz.getName()) })
}

trait FastTypeTagMacros extends Macro {
  def impl[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val T = weakTypeOf[T]
    q"""
      new FastTypeTag[$T] {
        def mirror = scala.pickling.internal.`package`.currentMirror
        lazy val tpe = scala.reflect.runtime.universe.weakTypeOf[$T]
        def key = ${T.key}
      }
    """
  }
  def implClassTag[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val T = weakTypeOf[T]
    q"""
      new FastTypeTag[ClassTag[$T]] {
        def mirror = scala.pickling.internal.`package`.currentMirror
        lazy val tpe = scala.reflect.runtime.universe.weakTypeOf[ClassTag[$T]]
        def key = "ClassTag[" + ${T.key} + "]"
      }
    """
  }
  def apply(key: c.Tree): c.Tree = {
    import c.universe._
    q"""scala.pickling.FastTypeTag(scala.pickling.internal.`package`.currentMirror, $key)"""
  }
}
