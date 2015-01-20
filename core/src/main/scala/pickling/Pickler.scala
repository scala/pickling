package scala.pickling

import scala.language.experimental.macros

import scala.annotation.implicitNotFound


/** A static pickler for type `T`. Its `pickle` method takes an object-to-be-pickled of
 *  static type `T`, and pickles it to an instance of `PBuilder`. In the process the object
 *  is turned into some external representation like a byte array. The particular external
 *  representation (the "pickle format") is defined by the `builder`.
 *
 *  This pickler requires that the dynamic type of the object-to-be-pickled is equal to
 *  the erasure of its static type `T`.
 */
@implicitNotFound(msg = "Cannot generate a pickler for ${T}. Recompile with -Xlog-implicits for details")
trait SPickler[T] {
  /** Uses the given builder to place 'primitive' values, or collections/structures, into the
   *  builder.
   */
  def pickle(picklee: T, builder: PBuilder): Unit
  /** The fast type tag associated with this pickler. */
  def tag: FastTypeTag[T]
}

/** A dynamic pickler for type `T`. Its `pickle` method takes an object-to-be-pickled of
 *  static type `T`, and pickles it to an instance of `PBuilder`. In the process the object
 *  is turned into some external representation like a byte array. The particular external
 *  representation (the "pickle format") is defined by the `builder`.
 *
 *  In contrast to static picklers (instances of type `SPickler[T]`), a dynamic pickler of
 *  type `DPickler[T]` pickles any object of type `T`.
 */
@implicitNotFound(msg = "Cannot generate a DPickler for ${T}. Recompile with -Xlog-implicits for details")
trait DPickler[T] {
  def pickle(picklee: T, builder: PBuilder): Unit
}

object DPickler {
  implicit def genDPickler[T]: DPickler[T] = macro Compat.PicklerMacros_dpicklerImpl[T]
}

trait GenPicklers {
  implicit def genPickler[T]: SPickler[T] = macro Compat.PicklerMacros_impl[T]
}

// marker trait to indicate a generated pickler
// this is important for the dispatch between custom and generated picklers
trait Generated

/** This is something which knowns how to reconstitute/materialize a type out of
 *  a pickle reader.
 */
@implicitNotFound(msg = "Cannot generate an unpickler for ${T}. Recompile with -Xlog-implicits for details")
trait Unpickler[T] {
  // TODO - we'd like  to call this method unpickeRaw and the unpickleEntry method `unpickle`,
  //        as there is some logic about how to use the reader encoded here.
  /** Unpickles an entry out of hte reader.
   *  
   *  note:  This method ASSUMES beginEntry() has already been called and endEntry() will be
   *         called immediately afterwards.
   *
   * @param tag  The FastTypeTag[_].key that was serialized with the entry *or* the type hint
   *             which was provided when reading.  This is generally used by abstract type 
   *             Unpicklers to delegate to the appropriate concrete unpickler.
   * @param reader  The reader we can grab fields, primitives or collection items out of.
   * @return Any an instance of the type we've unpickled.
   */
  def unpickle(tag: String, reader: PReader): Any
  /** A mechanism of unpickling that also includes calling beginEntry()/endEntry(). 
   *  Note: We assume anyone calling this will hint "staticallyElided" or "dynamicallyElided"
   *        if needed.   Each Unpickler should make no assumptions about its own type.
   */
  def unpickleEntry(reader: PReader): Any = {
    reader.hintTag(this.tag)
    val tag = reader.beginEntry()
    val result = unpickle(tag, reader)
    reader.endEntry()
    result
  }
  /** The fast type tag associated with this unpickler. */
  def tag: FastTypeTag[T]
}

/** Open sum pickler methods.
 *
 *  Open-sum means that we don't have a "closed" object hierarchy, and we need to resort
 *  to runtime reflection to handle classes which we didn't know about when we compiled
 *  the unpicler.
 */
trait GenOpenSumUnpicklers {
  /** Generates an unpickler which can use runtime reflection to unpickle classes
   * which were not known when this Unpickler was created.
   *
   * The unpickler will attempt to use as much static information as possible.
   * While the [[GenUnpicklers.genUnpickler]] macro is able to be run in static-only mode,
   * this macro *must* always resort to runtime reflection.
   */
  implicit def genOpenSumUnpickler[T]: Unpickler[T] with Generated = macro Compat.OpenSumUnpicklerMacro_impl[T]
}

trait GenUnpicklers extends GenOpenSumUnpicklers {
  /** Generates an unpickler which can rematerialize the given type.
   *
   *  Note: If this unpickler encounters a type the compiler does not know about, it will
   *        resort to runtim reflection.  This can be disabled by ensuring you:
   *        {{{ import scala.pickling.static.StaticOnly}}}
   *        everywhere this macro is used.
   *
   */
  implicit def genUnpickler[T]: Unpickler[T] with Generated = macro Compat.UnpicklerMacros_impl[T]
}
