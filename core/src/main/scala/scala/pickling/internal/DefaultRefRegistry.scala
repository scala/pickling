package scala.pickling.internal

import java.util.IdentityHashMap

import scala.pickling.PicklingException
import scala.pickling.spi.{RefUnpicklingRegistry, RefPicklingRegistry, RefRegistry}

/** Default implementation of the Ref registry that allows circular dependencies to be handled.
  * Uses thread-local caches (per pickler/unpickler thread).
  */
final class DefaultRefRegistry extends RefRegistry {
  private object picklerTl extends ThreadLocal[RefPicklingRegistry] {
    override def initialValue(): RefPicklingRegistry = new DefaultRefPicklingRegistry
  }
  private object unpicklerTl extends ThreadLocal[RefUnpicklingRegistry] {
    override def initialValue(): RefUnpicklingRegistry = new DefaultRefUnpicklingRegistry()
  }
  override def pickle: RefPicklingRegistry = picklerTl.get()
  override def unpickle: RefUnpicklingRegistry = unpicklerTl.get()
}
class DefaultRefPicklingRegistry extends RefPicklingRegistry {
  private val refs = new IdentityHashMap[AnyRef, Integer]()
  private var nextPicklee: Int = 0
  override def registerPicklee(picklee: Any): Int = {
    val anyRefPicklee = picklee.asInstanceOf[AnyRef]
    // check if `anyRefPicklee` is already in the map.
    // if so, obtain its index, else insert at index `nextPicklee`.
    if (refs.containsKey(anyRefPicklee)) {
      refs.get(anyRefPicklee).intValue
    } else {
      refs.put(anyRefPicklee, new Integer(nextPicklee))
      nextPicklee = nextPicklee + 1
      -1
    }
  }
  override def clear(): Unit = {
    refs.clear()
    nextPicklee = 0
  }
}

// Single-threaded unpickling registry. */
class DefaultRefUnpicklingRegistry(maxRefs: Int = 655536) extends RefUnpicklingRegistry {
  private var refs: Array[Any] = new Array[Any](maxRefs)
  private var idx = 0
  override def preregisterUnpicklee(): Int = {
    val index = idx
    val len = refs.length
    val target = if (index == len) {
      val newArr = Array.ofDim[Any](len * 2)
      System.arraycopy(refs, 0, newArr, 0, len)
      refs = newArr
      newArr
    } else refs
    target(index) = null
    idx += 1
    index
  }
  override def clear(): Unit = {
    val last = idx
    idx = 0
    var i = 0
    while (i < last) {
      refs(i) = null
      i += 1
    }

  }
  override def regsiterUnpicklee(oid: Int, value: Any): Unit = {
    refs(oid) = value
  }
  override def lookupUnpicklee(oid: Int): Any = {
    if (oid >= idx) throw PicklingException(s"fatal error: invalid index $oid unpicklee cache of length $idx")
    val result = refs(oid)
    if (result == null) throw new Error(s"fatal error: unpicklee cache is corrupted at $oid")
    result
  }
}

