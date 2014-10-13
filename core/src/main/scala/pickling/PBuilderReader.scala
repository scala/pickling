package scala.pickling

import scala.language.experimental.macros

import scala.reflect.runtime.universe._

trait Hintable {
  def hintTag(tag: FastTypeTag[_]): this.type
  def hintKnownSize(knownSize: Int): this.type
  def hintStaticallyElidedType(): this.type
  def hintDynamicallyElidedType(): this.type
  def hintOid(id: Int): this.type
  def pinHints(): this.type
  def unpinHints(): this.type
  def pushHints(): this.type
  def popHints(): this.type
}

trait PBuilder extends Hintable {
  def beginPickle(): PBuilder
  def beginEntry(picklee: Any): PBuilder
  def putField(name: String, pickler: PBuilder => Unit): PBuilder
  def endEntry(): Unit
  def beginCollection(length: Int): PBuilder
  def putElement(pickler: PBuilder => Unit): PBuilder
  def endCollection(): Unit
  def result(): Pickle
}

trait PReader extends Hintable {
  def mirror: Mirror
  def beginPickle(): PReader
  def beginEntry(): FastTypeTag[_]
  def beginEntryNoTag(): String
  def beginEntryNoTagDebug(debugOn: Boolean): String
  def atPrimitive: Boolean
  def readPrimitive(): Any
  def atObject: Boolean
  def readField(name: String): PReader
  def endEntry(): Unit
  def beginCollection(): PReader
  def readLength(): Int
  def readElement(): PReader
  def endCollection(): Unit
}

case class PicklingException(msg: String) extends RuntimeException(msg)

class EndOfStreamException extends RuntimeException
