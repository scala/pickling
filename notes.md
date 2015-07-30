# Interanl/Runtime redesign notes.



## Usages of internal/runtime services.

* Ref registration/lookup
  - Unpickle
    - `_root_.scala.pickling.internal.package.preregisterUnpicklee()`
    - `_root_.scala.pickling.internal.package.registerUnpicklee($instance, oid)`
  - Pickle
    - `_root_.scala.pickling.internal.package.lookupPicklee(picklee)`
* Current Runtime Mirror
  - FastTypeTag
    - `_root_.scala.pickling.internal.package.currentMirror`
  - Dynamic Unpicklers
    - `_root_.scala.pickling.internal.package.currentMirror`
* Runtime Pickler Generation
  - Unpickle
    - `_root_.scala.pickling.runtime.RuntimeUnpicklerLookup.genUnpickler(_root_.scala.pickling.internal.package.currentMirror, tagKey)`
  - Pickle
    - `_root_.scala.pickling.runtime.RuntimePicklerLookup.genPickler`
* Global Refelection Lock
  - Pickel
     - `_root_.scala.pickling.internal.GRL.lock()`
* CAVEAT - The `Ref` unpickler should probably actually just be a direct `runtime` call, rather than what happens now.
     
     
     
## Proposal


```scala
// Note: The RefRegistry should actually be a TLS, and handle a specific pickle/unpickle call.
trait RefRegistry {
  // Pickling
  def registerPicklee(picklee: Any): Int
  // Unpickling
  def preregisterUnpicklee(): Int
  def regsiterUnpicklee(oid: Int, value: Any): Unit
  def lookupUnpicklee(oid: Int): Option[Any]
  // Removes all instances from the registry.  This should be done AFTER a top-level unpickle/pickle call.
  def clear(): Unit
}
```

```scala
// User-Facing API (for making picklers)
// Each of these methods NEEDS to be 100% thread-safe.
trait PicklerRegistry {
  def lookupUnpickler[T]: Unpickler[T]
  def lookupPickler[T]: Pickler[T]
  def registerPickler[T](p: Pickler[T]): Unit
  def registerUnpickler[T](p: Unpickler[T]): Unit
  // TODO - Some kind of clean or inspect what we have?
}
```

```scala
trait PicklingRuntime {
  //
  // Ideally these do not show up at all.
  def currentMirror: Mirror
  def GRL: GRL.type
  //
  // User-Facing API (for making picklers)
  // Each of these methods NEEDS to be 100% thread-safe.
  def makeFastTag[T](tagKey: String): FastTypeTag[T]
  def picklers: PicklerRegistry
  //
  // User-Facing API for `Ref` and circular reference handling
  def references: RefRegistry
  //
  // Provider API?
  // TODO - A mechanism to replace the runtime-pickler-generator
}
object PicklingRuntime {
  def current: PicklingRuntime = ...
  // Note: This would need to have a way to ensure registered picklers can be obtained from the previous, in the
  //       event we do static initialization...
  def replace(next: PicklingRuntime): PicklingRuntime = ...
}
```