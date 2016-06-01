package scala.pickling
package pickler

/** All pickler instances, including the low priority implicits. */
trait AllPicklers extends LowPriorityPicklers
  with PrimitivePicklers
  with NothingPicklers
  with DatePicklers
  with JavaBigDecimalPicklers
  with JavaBigIntegerPicklers
  with JavaUUIDPicklers
  with TypeTagPicklers
  with PrimitiveArrayPicklers
  with RefPicklers
  with EitherPicklers
  with CollectionPicklers {}
object AllPicklers extends AllPicklers {}

/** All collection picklers except [[List]] which is handled by a macro. */
trait CollectionPicklers extends MutableMapPicklers
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
  with IterablePicklers {}

// Force `Any` to be the last pickler to be picked in the implicit search
trait LowPriorityPicklers extends GenPicklersUnpicklers with AnyUnpicklers {}
