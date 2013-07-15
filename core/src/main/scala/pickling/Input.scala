package scala.pickling

import scala.language.experimental.macros

trait DecodingInput {

  def decodeByteFrom(pos: Int): (Byte, Int)

  def decodeShortFrom(pos: Int): (Short, Int)

  def decodeCharFrom(pos: Int): (Char, Int)

  def decodeIntFrom(pos: Int): (Int, Int)

  def decodeLongFrom(pos: Int): (Long, Int)

  def decodeStringFrom(pos: Int): (String, Int)

  def decodeBooleanFrom(pos: Int): (Boolean, Int)

  def decodeIntArrayFrom(pos: Int): (Array[Int], Int)

}
