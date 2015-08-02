package scala.pickling
package spi


/** The is the interface used by picklers to register/handle circular references in picklees.
  *
  * This interface is stateful, and we assume only one thread will talk to it at a time.
  */
trait RefPicklingRegistry {
  /** Returns the OID of the picklee.
    * @param picklee An object to tag/track
    * @return A -1 if this object hasn't been seen since the last clear on this thread *OR*
    *         the previous OID if we have seen it before.
    */
  def registerPicklee(picklee: Any): Int
  /** Clears all registered objects out of the this cache. */
  def clear(): Unit
}

/** This is the interface used by unpicklers to register/handle `Ref` types when unpickling.
  *
  * We assume only one thread will be talking to a RefUnpicklingRegistry at a time.
  */
trait RefUnpicklingRegistry {
  /** Grabs the registeration id for the next object. */
  def preregisterUnpicklee(): Int
  /** Registers an object to an id, after its FIRST deserialization. */
  def regsiterUnpicklee(oid: Int, value: Any): Unit
  /** Looks up an unpicklee by its object id. Throws an exception if oid is not valid. */
  def lookupUnpicklee(oid: Int): Any
  /** Removes all instances from the registry.  This should be done AFTER a top-level unpickle/pickle call. */
  def clear(): Unit

}

/** The owner of `Ref` registeries. These are used to help detangle circular references/trees when pickling or to
  * help optimise/reduce the amount of data pickled if an object shows up more than once.
  */
trait RefRegistry {
  /** Returns the pickling registry for this thread. */
  def pickle: RefPicklingRegistry
  /** Returns the unpickling registry for this thread. */
  def unpickle: RefUnpicklingRegistry
}