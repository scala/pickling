package scala.pickling
package spi

import scala.reflect.runtime.universe.Mirror

/** A registry for looking up (and possibly coding on the fly) picklers by tag.
  *
  * All methods are threadsafe.
  */
trait PicklerRegistry {
  // TODO(jsuereth) - We should remove the `gen` traits here and hide generation behind the lookup methods.

  /** Looks up the registered unpickler using the provided tagKey.
    *
    * If there are no registered picklers or pickler-generators, then we instead attempt to generate the pickler using
    * the passed in information.
    *
    * TODO(jsuereth) - This should use classLoader just like genPickler.  No reason to mix Java/Scala reflection.
    *
    * @param mirror  The scala mirror (classloader/symbolloader) we use to generate the unpickler.
    * @param tagKey The full tag of the type, which may or may not include type parameters.
    */
  def genUnpickler(mirror: Mirror, tagKey: String)(implicit share: refs.Share): Unpickler[_]
  /** Looks up a Pickler for the given tag.  If none is found, then we attempt to generate one.
    *
    * @param classLoader The classloader to use when reflecting over the pickled class.
    * @param clazz The clazz we need to pickle.
    * @param tag The full tag of the type we're pickling, which may or may not include type parameters.
    */
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_]

  /** Checks if lookup is enabled for this registry */
  def isLookupEnabled: Boolean

  /** Checks the existince of an unpickler.
    *
    * This will also check any registered generator functions.
    */
  def lookupUnpickler(key: String): Option[Unpickler[_]]

  /** Looks for a pickler with the given FastTypeTag string.
    *
    * This will also check any registered generator functions.
    */
  def lookupPickler(key: String): Option[Pickler[_]]

  /** Checks the existence of an unpickler ignoring the registered generators. */
  def lookupExistingUnpickler(key: String): Option[Unpickler[_]]

  /** Checks the existence of a pickler ignoring the registered generators. */
  def lookupExistingPickler(key: String): Option[Pickler[_]]

  /** Registers a pickler with this registry for future use.
    *
    * @param key  The type key for the pickler. Note: In reflective scenarios this may not include type parameters.
    *             In those situations, the pickler should be able to handle arbitrary (existential) type parameters.
    * @param p  The pickler to register.
    */
  def registerPickler[T](key: String, p: Pickler[T]): Unit
  def registerPickler[T](p: Pickler[T]): Unit = registerPickler(p.tag.key, p)
  /** Registers an unpickler with this registry for future use.
    * @param key  The type key for the unpickler. Note: In reflective scenarios this may not include type parameters.
    *             In those situations, the unpickler should be able to handle arbitrary (existential) type parameters.
    * @param p  The unpickler to register.
    */
  def registerUnpickler[T](key: String, p: Unpickler[T]): Unit
  def registerUnpickler[T](p: Unpickler[T]): Unit = registerUnpickler(p.tag.key, p)
  /** Registers a pickler and unpickler for a type with this registry for future use.
    * @param key  The type key for the pickler. Note: In reflective scenarios this may not include type parameters.
    *             In those situations, the pickler should be able to handle arbitrary (existential) type parameters.
    * @param p  The unpickler to register.
    */
  def registerPicklerUnpickler[T](key: String, p: (Pickler[T] with Unpickler[T])): Unit
  def registerPicklerUnpickler[T](p: Pickler[T] with Unpickler[T]): Unit = registerPicklerUnpickler(p.tag.key, p)


  /** Registers a function which can generate picklers for a given type constructor.
    *
    * @param typeConstructorKey  The type constructor.  e.g. "scala.List" for something that can make scala.List[A] picklers.
    * @param generator  A function which takes an applied type string (your type + arguments) and returns a pickler for
    *                   this type.
    *                   Note:  it is possible for the type arguments to be an empty set.  This is the case if we are
    *                   trying to manually inspect an object at runtime to deterimine its type, and we do not know what
    *                   the arguments are.  You can treat this case as 'existential' arguments.
    */
  def registerPicklerGenerator[T](typeConstructorKey: String, generator: FastTypeTag[_] => Pickler[T]): Unit
  /** Registers a function which can generate picklers for a given type constructor.
    *
    * @param typeConstructorKey  The type constructor.  e.g. "scala.List" for something that can make scala.List[A] picklers.
    * @param generator  A function which takes an applied type string (your type + arguments) and returns a pickler for
    *                   this type.
    */
  def registerUnpicklerGenerator[T](typeConstructorKey: String, generator: FastTypeTag[_] => Unpickler[T]): Unit
  /** Registers a function which can generate picklers for a given type constructor.
    *
    * @param typeConstructorKey  The type constructor.  e.g. "scala.List" for something that can make scala.List[A] picklers.
    * @param generator  A function which takes an applied type string (your type + arguments) and returns a pickler for
    *                   this type.
    */
  def registerPicklerUnpicklerGenerator[T](typeConstructorKey: String, generator: FastTypeTag[_] => (Pickler[T] with Unpickler[T])): Unit

  /** Clear the registered pickler/unpickler for a given type.
    *
    * Useful for avoiding conflict between picklers registered with different
    * sharing strategies and are cached when they're initialised.
    */
  private[pickling] def clearRegisteredPicklerUnpicklerFor[T: FastTypeTag]: Unit

}
