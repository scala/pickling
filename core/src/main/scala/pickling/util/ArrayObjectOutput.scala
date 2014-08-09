package scala.pickling.util

import java.io.ObjectOutput

/** An `ObjectOutput` that enables access to the written data using array-valued fields.
 */
trait ArrayObjectOutput extends ObjectOutput {
  def booleanArr: Array[Boolean]
  def byteArr: Array[Int]
  def charArr: Array[Int]
  def doubleArr: Array[Double]
  def floatArr: Array[Float]
  def intArr: Array[Int]
  def longArr: Array[Long]
  def shortArr: Array[Int]
  def arrByteArr: Array[Array[Byte]]
  def anyRefArr: Array[Any]
}
