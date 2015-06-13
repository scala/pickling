package scala.pickling

import scala.reflect.runtime.universe.Mirror

/** The location (buffer or stream) where values may be serialized.
  *
  * TODO - We may want to rethink this interface a bit.  e.g. PBuilder can always return a pickle now. HOwever for
  *        directly streamed PBuilder's we realyl don't want to do that.
  */
trait Pickle {
  /** The type of values stored in this pickle. */
  type ValueType
  /* The value currently stored in this pickle. */
  val value: ValueType
}

/**
 * A format for how to pickle the structure of an object.
 */
trait PickleFormat {
  /** The type of the pickle, which stores the content of the object. */
  type PickleType <: Pickle
  /** An output through which we can serialize objects in a streaming fashion. */
  type OutputType
  /** Create a PBuilder which will serialize objects in memory, through a Pickle object. */
  def createBuilder(): PBuilder
  /** Create a PBuilder which should serialize objects into the output directly. */
  def createBuilder(out: OutputType): PBuilder
  /** Create a reader which can take a pickle and create a structured reader for the pickle. */
  def createReader(pickle: PickleType): PReader
}
