package scala.pickling.debug

import scala.pickling.{PBuilder, StringOutput, Output, PickleFormat}

/**
 * A pickle format which throws exceptions when you violate rules of implementing picklers, used so you can test
 * custom picklers/unpicklers.
 */
class DebugPickleFormat[F <: PickleFormat](val underlying: F) extends PickleFormat {
  type PickleType = underlying.PickleType
  type OutputType = underlying.OutputType
  def createBuilder() =
    new DebugPBuilder(underlying.createBuilder())
  def createBuilder(out: OutputType): PBuilder =
    new DebugPBuilder(underlying.createBuilder(out))
  def createReader(pickle: underlying.PickleType) =
    ???
}