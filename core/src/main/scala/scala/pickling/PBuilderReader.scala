package scala.pickling

import scala.language.experimental.macros

import scala.reflect.runtime.universe._

/**
 * Hintable defines the interface used between picklers and formats to "aide' in creating clean/efficient formats.
 *
 *
 * The FULL features picklers allow:
 * - eliding statically known types
 * - structural sharing of data
 * - possible binary size optimizations (using jvm sizes)
 *
 * Obviously not all picklers will use these mechanisms.
 *
 * ----
 * === Size Optimization ===
 * If a pickler/unpickler calls `hintKnownSize`, it's talking about a binary size of the following entry.
 * ----
 * === Type Optimization ===
 * If a pickler/unpickler call `hintElidedType`, it's allowing the underlying format to 'drop' the forced storage of a
 * type tag.
 * ----
 * === Structural Sharing ===
 * If a pickler/unpickler calls `hintOid`, it's telling the underlying format that this particular pickler entry
 * shares it structure with the same entry of that number.
 *
 * Currently, rather than using known tags, picklers assume a per-pickle ordinal associated with each entry.  This
 * ordinal should be automatically tracked and restored.
 *
 */
trait Hintable {
  /** Hints at the expected (byte) size of the entry we're about to write.. */
  def hintKnownSize(knownSize: Int): this.type
  /** Hints to the pickle format that we KNOW the type of this field, so it can elide any type hints from the final pickle.
    *  During unpickling, this informs the format that it shouldn't look for any type hints in the pickle, but instead
    *  use this tag as the unpickle tag key.
    */
  def hintElidedType(tag: FastTypeTag[_]): this.type

  /** Hints the object id of the next `beginEntry`/`endEntry` calls.
    * If this oid is -1, it implies that the next entry has not been registered in any sharing cache, and should
    * be pickled fully.   If the oid != -1, then the pickle format is free to elide the fields of the entry in
    * favor of pickling a [[scala.pickling.refs.Ref]].
    *
    * Hint: This is only used during pickling.
    */
  def hintOid(id: Int): this.type
  /** Locks the hints down to what we've specified, so if you drop into another pickler it retains our information. */
  def pinHints(): this.type
  /** Unlocks the hints. */
  def unpinHints(): this.type
  /** Creates a new fresh set of hints, preserving what was hinted before. */
  def pushHints(): this.type
  /** Pops to the previously saved set of hints. */
  def popHints(): this.type
}

/**
 * A builder of pickled content.  This is a mutable API, intended to be called in certain specific ways.
 *
 * Here are a few static rules that all picklers must follow when using this interface.
 *
 * 1. There will be one endEntry() for every beginEntry() call.
 * 2. There will be one endCollection() for every beginCollection() call.
 * 3. Every beginCollection()/endCollection() pair will be inside a beginEntry()/endEntry() pair.
 * 4. Every putElement() call must happen within a beginCollection()/endCollection() block.
 * 5. Every putField() call must happen within a beginEntry()/endEntry() block.
 * 6. There is no guarantee that putElement() will be called within a beginCollectoin()/endCollection() pair.
 *    i.e. we can write empty collections.
 * 7. There is no guarantee that putField will be called within a beginEntry()/endEntry() pair.
 *    i.e. if we don't put any fields, this means the entry was for a "primitive" type, at least what
 *    The pickling library considers primitives.
 * 8. The order of putField calls in any pickler will be the exact same ordering when unpickling, if the format
 *    is compatible.
 *
 * Here is a list of all types the auto-generated Picklers considers "primitives" and must be directly supported by
 * any PBuilder:
 *
 *   - Nothing
 *   - Null
 *   - Unit
 *   - Byte
 *   - Char
 *   - String
 *   - Short
 *   - Int
 *   - Long
 *   - Float
 *   - Double
 *   - Ref  (for circular object graphs)
 *   - ArrayByte
 *   - ArrayShort
 *   - ArrayChar
 *   - ArrayInt
 *   - ArrayLong
 *   - ArrayBoolean
 *   - ArrayFloat
 *   - ArrayDouble
 */
trait PBuilder extends Hintable {
  /** Called to denote that an object is about to be serialized.
    * @param picklee
    *                The object to be serialized.  This may be a primtiive, in which case
    *                it can be immediately serialized (or you can wait unitl endEntry is called).
    * @param tag
    *                The tag to use when pickling this entry.   Tags must be serialized/restored, unless
    *                otherwise hinted that it can be elided.
    * @return
    *                A pbuilder instance a pickler can use to serialize the picklee, if it's a complex type.
    */
  def beginEntry(picklee: Any, tag: FastTypeTag[_]): PBuilder
  /**
   * Serialize a "field" in a complex structure/object being pickled.
   * @param name  The name of the field to serialize.
   * @param pickler  A callback which will be passed an appropriate pickler.
   *                 You should ensure this function will perform a beginEntry()/endEntry() block.
   * @return A builder for remaining items in the current complex structure being pickled.
   */
  def putField(name: String, pickler: PBuilder => Unit): PBuilder

  /**
   * Call this to denote that the given primitive, collection or structure being pickled is completed.
   */
  def endEntry(): Unit

  /**
   * Denotes that a collection of elements is about to be pickled.
   *
   * Note: This must be called after beginEntry()
   * @param length   The length of the collection being serialized.
   * @return  A pickler which can serialzie the collection.
   */
  def beginCollection(length: Int): PBuilder

  /**
   * Places the next element in the serialized collection.
   *
   * Note: This must be called after beginCollection().
   * @param pickler  A callback which is passed a pickler able to serialize the item in the collection.
   * @return  A pickler which can serialize the next element of the collection.
   */
  def putElement(pickler: PBuilder => Unit): PBuilder
  /** Denote that we are done serializing the collection. */
  def endCollection(): Unit
  /** Return the resulting pickle of this builder. */
  def result(): Pickle
}
// Abstract shim for Java.
abstract class AbtsractPBuilder extends PBuilder with PickleTools



/**
 * A reader of pickled content.  This is a mutable API, intended to be called in certain specific ways.
 *
 * Here are a few static rules that all picklers must follow when using this interface.
 *
 * 1. There will be one endEntry() for every beginEntry() call.
 * 2. There will be one endCollection() for every beginCollection() call.
 * 3. Every beginCollection()/endCollection() pair will be inside a beginEntry()/endEntry() pair.
 * 4. Every readLength() call will be immediately after a beginCollection() call.
 * 5. Every readElement() call must happen within a beginCollection()/endCollection() block, and after a readLength().
 * 6. Every readField() call must happen within a beginEntry()/endEntry() block.
 * 7. If readLength() returns 0, there will be no called to readElement().
 * 8. readField() will only be called where atObject would return true
 * 9. readPrimitive will only be called when atPrimitive would return true
 * 10. The order of readField calls in any pickler will be the exact same ordering when pickling,
 *
 * Here is a list of all types the auto-generated Picklers considers "primitives" and must be directly supported by
 * any PReader "readPrimitive" operation:
 *
 *   - Nothing
 *   - Null
 *   - Unit
 *   - Byte
 *   - Char
 *   - String
 *   - Short
 *   - Int
 *   - Long
 *   - Float
 *   - Double
 *   - Ref  (for circular object graphs)
 *   - ArrayByte
 *   - ArrayShort
 *   - ArrayChar
 *   - ArrayInt
 *   - ArrayLong
 *   - ArrayBoolean
 *   - ArrayFloat
 *   - ArrayDouble
 */
trait PReader extends Hintable {
  /** Start reading a pickled value.  
   *  This will return any serialized type tag key string.   This string can be used
   *  to reconstitute a FastTypeTag w/ a mirror, but is intended for use as fast string-matching.
   */
  def beginEntry(): String
  /** returns true if the reader is currently looking at a pickled primitive. */
  def atPrimitive: Boolean
  /** Reads one of the supported primitive types from the pickler. */
  def readPrimitive(): Any
  /** returns true if the reader is currently looking at a pickled object/structure. */
  def atObject: Boolean
  /** Returns a reader which can read a field of
    * a complex structure in the pickle.
    * @param name  The name of the field
    * @return  A reader which can read the structure's field.
    */
  def readField(name: String): PReader
  /** Denotes that we're done reading an entry in the pickle. */
  def endEntry(): Unit
  /** Denotes we'd like to read the current entry as a collection.
    * Note: Must be called after a beginEntry* call.
    */
  def beginCollection(): PReader
  /** Reads the length of a serialized collection.
    * Must be called directly after beginCollection and before readElement.
    * @return  The length of a serialized collection.
    */
  def readLength(): Int
  /** Returns a new Reader that can be used to read the next element in a collection.  */
  def readElement(): PReader
  /** Denote that we are done reading a collection. */
  def endCollection(): Unit
}

// Abstract class for Java implementors of picklers.
abstract class AbstractPReader extends PReader with PickleTools

/**
 * Exception thrown when the pickling or unpickling process fails.
 * @param message error message
 * @param cause exception causing the pickling exception if any
 */
final case class PicklingException(message: String, cause: Option[Throwable] = None) extends RuntimeException(message, cause.orNull)

/**
 * Exception thrown when a stream ends unexpectedly during unpickling.
 */
final class EndOfStreamException extends RuntimeException("End of stream reached while unpickling")
