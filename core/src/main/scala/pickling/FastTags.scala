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
  implicit def materializeFastTypeTag[T]: FastTypeTag[T] = macro materializeImpl[T]
  def materializeImpl[T: c.WeakTypeTag](c: Context): c.Expr[FastTypeTag[T]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr[FastTypeTag[T]](bundle.materializeImpl[T])
  }

  private def stdTag[T: ru.TypeTag]: FastTypeTag[T] = apply(scala.reflect.runtime.currentMirror, ru.typeOf[T], ru.typeOf[T].key).asInstanceOf[FastTypeTag[T]]

  val Null    = stdTag[Null]
  val Byte    = stdTag[Byte]
  val Short   = stdTag[Short]
  val Char    = stdTag[Char]
  val Int     = stdTag[Int]
  val Long    = stdTag[Long]
  val Boolean = stdTag[Boolean]
  val Float   = stdTag[Float]
  val Double  = stdTag[Double]
  val Unit    = stdTag[Unit]

  val ScalaString = stdTag[String]
  val JavaString = stdTag[java.lang.String]

  val ArrayByte = stdTag[Array[Byte]]
  val ArrayInt = stdTag[Array[Int]]
  val ArrayLong = stdTag[Array[Long]]
  implicit val Nothing: FastTypeTag[Nothing] = stdTag[Nothing]

  def apply(mirror0: ru.Mirror, tpe0: ru.Type, key0: String): FastTypeTag[_] = {
    new FastTypeTag[Nothing] {
      def mirror = mirror0
      def tpe = tpe0
      def key = key0
    }
  }

  def apply(mirror: ru.Mirror, key: String): FastTypeTag[_] = apply(mirror, typeFromString(mirror, key), key)
  def apply(key: String): FastTypeTag[_] = macro applyImpl
  def applyImpl(c: Context)(key: c.Expr[String]): c.Expr[FastTypeTag[_]] = {
    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with FastTypeTagMacros
    c.Expr[FastTypeTag[_]](bundle.applyImpl(key.tree))
  }
}

trait FastTypeTagMacros extends Macro {
  def materializeImpl[T: c.WeakTypeTag]: c.Tree = {
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
  def applyImpl(key: c.Tree): c.Tree = {
    import c.universe._
    q"""scala.pickling.FastTypeTag(scala.pickling.`package`.currentMirror, $key)"""
  }
}
