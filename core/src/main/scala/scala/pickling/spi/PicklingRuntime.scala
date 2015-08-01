package scala.pickling.spi

import java.util.concurrent.locks.ReentrantLock

import scala.pickling.{Pickler, Unpickler, FastTypeTag}
import scala.reflect.runtime.universe.Mirror


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

/** A registry for looking up (and possibly coding on the fly) picklers by tag.
  *
  * All methods are threadsafe.
  */
trait PicklerRegistry {
  /** Looks up the registered unpickler using the provided tagKey. */
  def lookupUnpickler(tagKey: String): Unpickler[_]
  /** Looks up a Pickler or generates one. */
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_]): Pickler[_]
  /** Registers a pickler with this registry for future use. */
  def registerPickler[T](p: Pickler[T]): Unit  // TODO - Return old pickler if one existed?
  /** Registers an unpickler with this registry for future use. */
  def registerUnpickler[T](p: Unpickler[T]): Unit
  // TODO - Some kind of clean or inspect what we have?
}

/**
 * This trait provides access to all the runtime services used in pickling.
 */
trait PicklingRuntime {
  /** Lock used to secure reflection usage. */
  def GRL: ReentrantLock
  /** Gives access to the current refRegistry. */
  def refRegistry: RefRegistry
  /** Creates a new fastTypeTag with the given tagKey. */
  def makeFastTag[T](tagKey: String): FastTypeTag[T]
  /** A registry of picklers for runtime lookup/usage. */
  def picklers: PicklerRegistry

  /** The current reflection mirror to use when doing runtime unpickling/pickling. */
  //  TODO - Allow this to get modified!
  def currentMirror: Mirror
}
