package scala.pickling

import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
import java.io.OutputStream

trait Output[T] {

  def result(): T

  def put(obj: T): this.type

}

class StringOutput extends Output[String] {

  private val buf =
    new StringBuilder()

  def result(): String =
    buf.toString

  def put(obj: String): this.type = {
    buf ++= obj
    this
  }

  override def toString = buf.toString
}
