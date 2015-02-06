package scala.pickling

import scala.language.experimental.macros
import scala.language.implicitConversions

import scala.annotation.implicitNotFound
import scala.pickling.runtime.GlobalRegistry
import scala.pickling.internal._

/** A static pickler for type `T`. Its `pickle` method takes an object-to-be-pickled of
 *  static type `T`, and pickles it to an instance of `PBuilder`. In the process the object
 *  is turned into some external representation like a byte array. The particular external
 *  representation (the "pickle format") is defined by the `builder`.
 *
 *  This pickler requires that the dynamic type of the object-to-be-pickled is equal to
 *  the erasure of its static type `T`.
 */
@implicitNotFound(msg = "Cannot generate a pickler for ${T}. Recompile with -Xlog-implicits for details")
trait Pickler[T] {
  /** Uses the given builder to place 'primitive' values, or collections/structures, into the
   *  builder.
   */
  def pickle(picklee: T, builder: PBuilder): Unit
  /** The fast type tag associated with this pickler. */
  def tag: FastTypeTag[T]
}

object Pickler {
  def generate[T]: Pickler[T] = macro Compat.PicklerMacros_impl[T]
}

/** A dynamic pickler for type `T`. Its `pickle` method takes an object-to-be-pickled of
 *  static type `T`, and pickles it to an instance of `PBuilder`. In the process the object
 *  is turned into some external representation like a byte array. The particular external
 *  representation (the "pickle format") is defined by the `builder`.
 *
 *  In contrast to static picklers (instances of type `Pickler[T]`), a dynamic pickler of
 *  type `DPickler[T]` pickles any object of type `T`.
 */
@implicitNotFound(msg = "Cannot generate a DPickler for ${T}. Recompile with -Xlog-implicits for details")
trait DPickler[T] {
  def pickle(picklee: T, builder: PBuilder): Unit
}

object DPickler {
  implicit def genDPickler[T]: DPickler[T] = macro Compat.PicklerMacros_dpicklerImpl[T]
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
object Unpickler {
  def generate[T]: Unpickler[T] = macro Compat.UnpicklerMacros_impl[T]
}

object PicklerUnpickler {
  def apply[T](p: Pickler[T], u: Unpickler[T]): Pickler[T] with Unpickler[T] = new DelegatingPicklerUnpickler(p, u)
  def generate[T]: Pickler[T] with Unpickler[T] = macro Compat.PicklerUnpicklerMacros_impl[T]
  /** This is a private implementation of PicklerUnpickler that delegates pickle and unpickle to underlying. */
  private class DelegatingPicklerUnpickler[T](p: Pickler[T], u: Unpickler[T]) extends Pickler[T] with Unpickler[T] {
    // From Pickler
    override def pickle(picklee: T, builder: PBuilder): Unit = p.pickle(picklee, builder)
    // From Pickler and Unpickler
    override def tag: FastTypeTag[T] = p.tag
    // From Unpickler
    override def unpickle(tag: String, reader: PReader): Any = u.unpickle(tag, reader)
  }
}

abstract class AutoRegister[T: FastTypeTag](name: String) extends Pickler[T] with Unpickler[T] {
  debug(s"autoregistering pickler $this under key '$name'")
  GlobalRegistry.picklerMap += (name -> (x => this))
  val tag = implicitly[FastTypeTag[T]]
  debug(s"autoregistering unpickler $this under key '${tag.key}'")
  GlobalRegistry.unpicklerMap += (tag.key -> this)
}
