package scala.pickling
package pickler

/** All pickler instances.
 */
trait AllPicklers extends PrimitivePicklers
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

/** All picklers for collections with exception of List which is handled by macro.
  *
  * These need to be between the big picklers and the gen picklers.
 */
trait CollectionPicklers extends AllGenPicklers with MutableMapPicklers
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

// Gen picklers need to be BELOW the collection implicits so that we can use the collection ones.
trait AllGenPicklers extends LowPriorityPicklers
  with GenPicklers
  with GenUnpicklers {}

// We force any to be the last pickler.
trait LowPriorityPicklers extends AnyUnpicklers {}
