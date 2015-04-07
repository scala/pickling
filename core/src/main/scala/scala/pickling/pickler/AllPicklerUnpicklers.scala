package scala.pickling
package pickler

/** All pickler instances.
 */
trait AllPicklers extends PrimitivePicklers
  with DatePicklers
  with JavaBigDecimalPicklers
  with JavaBigIntegerPicklers
  with JavaUUIDPicklers
  with PrimitiveArrayPicklers
  with RefPicklers
  with GenPicklers
  with GenUnpicklers
  with LowPriorityPicklers {}
object AllPicklers extends AllPicklers {}

/** All picklers for collections with exception of List which is handled by macro.
 */
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

trait LowPriorityPicklers extends CollectionPicklers
  with AnyUnpicklers {}
