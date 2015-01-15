package scala.pickling

import scala.language.implicitConversions

package object binary {
  implicit val pickleFormat = new BinaryPickleFormat
  implicit def toBinaryPickle(value: Array[Byte]): BinaryPickle = BinaryPickle(value)
  implicit def binaryPickleToUnpickleOps(value: Array[Byte]): UnpickleOps = new UnpickleOps(BinaryPickle(value))
}
