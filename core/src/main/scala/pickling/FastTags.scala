package scala.pickling

import scala.reflect.macros.Context
import scala.reflect.api.{Universe => ApiUniverse}
import scala.reflect.runtime.{universe => ru}
import language.experimental.macros

trait FastTypeTag[T] extends Equals {
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
}

trait FastTypeTagMacros extends Macro {
  def impl[T: c.WeakTypeTag]: c.Tree = {
    import c.universe._
    val T = weakTypeOf[T]
    q"""
      new FastTypeTag[$T] {
        def mirror = scala.pickling.`package`.currentMirror
        lazy val tpe = scala.reflect.runtime.universe.typeTag[$T].tpe
        def key = ${T.key}
      }
    """
  }
  def apply(key: c.Tree): c.Tree = {
    import c.universe._
    q"""scala.pickling.FastTypeTag(scala.pickling.`package`.currentMirror, $key)"""
  }
}
