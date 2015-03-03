package scala.pickling.diagnostic

import scala.pickling.{PBuilder, StringOutput, Output, PickleFormat}

/**
 * A pickle format which throws exceptions when you violate rules of implementing picklers, used so you can test
 * custom picklers/unpicklers.
 */
class DiagnosticPickleFormat[F <: PickleFormat](val underlying: F) extends PickleFormat {
  type PickleType = underlying.PickleType
  type OutputType = underlying.OutputType
  def createBuilder() =
    new DiagnosticBuilder(underlying.createBuilder())
  def createBuilder(out: OutputType): PBuilder =
    new DiagnosticBuilder(underlying.createBuilder(out))
  def createReader(pickle: underlying.PickleType) =
    ???
}