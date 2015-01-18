package scala.pickling

import scala.reflect.runtime.universe.Mirror


trait Pickle {
  type ValueType

  val value: ValueType
}

trait PickleFormat {
  type PickleType <: Pickle
  type OutputType

  def createBuilder(): PBuilder
  def createBuilder(out: OutputType): PBuilder
  def createReader(pickle: PickleType): PReader
}
