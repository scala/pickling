package scala.pickling
package internal

import scala.pickling.spi.{RefRegistry, RefUnpicklingRegistry, RefPicklingRegistry}

/**
 * An implementation of ref sharing that ensures no references are created while pickling and none are looked
 * up while unpickling.
 */
final class NoSharingRefRegistry extends RefRegistry {
  override val pickle: RefPicklingRegistry = new NoSharingRefPicklingRegistry
  override val unpickle: RefUnpicklingRegistry = new NoSharingRefUnpicklingRegistry
}

/** An implementation which ensures that no newly generated picklers create shared references. */
final class NoPickleSharingRefRegistry extends RefRegistry {
  private object unpicklerTl extends ThreadLocal[RefUnpicklingRegistry] {
    override def initialValue(): RefUnpicklingRegistry = new DefaultRefUnpicklingRegistry()
  }
  override val pickle: RefPicklingRegistry = new NoSharingRefPicklingRegistry
  override def unpickle: RefUnpicklingRegistry = unpicklerTl.get()
}

final class NoSharingRefUnpicklingRegistry extends RefUnpicklingRegistry {
  /** Grabs the registeration id for the next object. */
  override def preregisterUnpicklee(): Int = -1

  /** Removes all instances from the registry.  This should be done AFTER a top-level unpickle/pickle call. */
  override def clear(): Unit = ()

  /** Registers an object to an id, after its FIRST deserialization. */
  override def regsiterUnpicklee(oid: Int, value: Any): Unit = ()

  /** Looks up an unpicklee by its object id. Throws an exception if oid is not valid. */
  override def lookupUnpicklee(oid: Int): Any =
    sys.error(s"Runtime reference sharing is disabled.  Your pickled object is trying to reference previously pickled value #$oid.")
}

/** An implementation of the RefRegistry which ensures NO sharing during pickling. */
final class NoSharingRefPicklingRegistry extends RefPicklingRegistry {
  override def registerPicklee(picklee: Any): Int = -1
  override def clear(): Unit = ()
}