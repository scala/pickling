package scala.pickling.pickler

import scala.pickling._

trait AllPicklers extends PrimitivePicklers
  with DatePicklers
  with JavaBigDecimalPicklers
  with JavaBigIntegerPicklers
  with PrimitiveArrayPicklers
  with RefPicklers
  with GenPicklers
  with GenUnpicklers
  with LowPriorityPicklers {
}
object AllPicklers extends AllPicklers {} 

trait LowPriorityPicklers extends MutableMapPicklers
  with ImmutableSortedMapPicklers
  with MapPicklers
  with MutableSortedSetPicklers
  with MutableSetPicklers
  with ImmutableSortedSetPicklers
  with SetPicklers
  with ArrayBufferPicklers
  with ArrayPicklers
  with VectorPicklers
  with LinearSeqPicklers
  with IndexedSeqPicklers
  with SeqPicklers
  with IterablePicklers
  with AnyUnpicklers {}
