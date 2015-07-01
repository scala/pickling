package scala.pickling

import scala.reflect.runtime.universe.Mirror

/** Holds the serialized representation of a value, such as an `Array[Byte]`.
  *
  * A `Pickle` is only returned by `PickleOps.pickle`. A directly-streamed `PBuilder` must be used together with
  * `PickleOps.pickleTo` or `PickleOps.pickleInto` which return `Unit` instead of a pickle.
  *
  * When unpickling from a stream, a subclass such as `BinaryInputPickle` is used, which initializes the `value`
  * to a dummy value. TODO - we may want to rethink this interface.
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
