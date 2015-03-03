package scala.pickling.debug

import scala.pickling._


private[debug] sealed trait BuilderState {
  def previous: BuilderState
  def underlying: PBuilder
}
private[debug] case class EmptyBuilderState(original: PBuilder) extends BuilderState {
  def previous = this
  def underlying = original
}
// The state which says we've returned a nested builder, and have not gotten a beginEntry/endEntry call yet.
private[debug] case class CompletedBuilderState(previous: BuilderState, underlying: PBuilder) extends BuilderState {}
private[debug] case class CollectionState(numElements: Int, previous: BuilderState, underlying: PBuilder) extends BuilderState {}
private[debug] case class RawEntryState(hints: Hints, var wasCollectionOrMap: Boolean = false, previous: BuilderState, underlying: PBuilder) extends BuilderState {}

/**
 * A PBuilder which will debug state-issues in generated SPicklers.  Can be used with any underlying builder.
 *
 * This will attempt to look for foul-play on the part of any pickler and ensure that all implicit rules of pickling are
 * met.
 */
class DebugPBuilder(orig: PBuilder)  extends PBuilder with PickleTools {

  var state: BuilderState = EmptyBuilderState(orig)


  /** The underlying PBuilder. */
  protected def underlying = state.underlying

  final def isValidFinalState: Boolean =  state match {
    case x: CompletedBuilderState => true
    case _ => false
  }

  override def result(): Pickle = {
    // TODO - make sure we don't have any orphaned child states, somehow.
    state match {
      case CompletedBuilderState(prev, underlying) => underlying.result()
      case other => sys.error(s"Cannot return pickler result when in transitory pickling state: $other")
    }
  }


  override def beginEntry(picklee: Any): PBuilder = withHints { hints =>
    // Rule #1 - Require a type hint before you can begin entry
    if(hints.tag == null) sys.error(s"Must call hintTag before beginEntry in PBuilder.\nhints = $hints, picklee = $picklee")
    state = RawEntryState(hints, previous = state, underlying = underlying.beginEntry(picklee))
    this
  }
  override def endEntry(): Unit = {
    state match {
      case rs: RawEntryState if !rs.wasCollectionOrMap =>
        // TODO - Enforce that the entry is for a primitive.
        // For some reason, string is not in the effectivelyprimitive list...
        if(!rs.hints.tag.isEffectivelyPrimitive && !(rs.hints.tag.key == "java.lang.String")) {
          // TODO - this may be a warning instead of an error.
          sys.error(s"Not safe to serialize (${rs.hints.tag.key}) as a primtive!, state: $state")
        }
        underlying.endEntry()
        state = state.previous
      case rs: RawEntryState =>
        // We got here from a beginCollection/endCollection call.  Now we just pop our state.
        underlying.endEntry()
        state = rs.previous
      case _ =>
        sys.error(s"Attempting to endEntry() from invalid state: $state")
    }
    // If we've hit the bottom fo the stack of work, we mark this pickler as being successful.
    if (state.isInstanceOf[EmptyBuilderState]) state = CompletedBuilderState(state, underlying)
  }


  override def putField(name: String, pickler: (PBuilder) => Unit): PBuilder =
    state match {
      case s: RawEntryState =>
        s.wasCollectionOrMap = true
        underlying.putField(name, pickler = { pu =>
          val next = new DebugPBuilder(pu)
          pickler(next)
          assert(next.isValidFinalState, s"Failed to correctly serialize field $name, bad state: ${next.state}}")
        })
        this
      case _ =>
        // Rule #6 - putField calls must occur immediately between beginEntry/endEntry
        sys.error(s"putField calls must occur between beginEntry/endEntry blocks.   Attempting to write field $name, w/ state: $state")
    }


  override def beginCollection(length: Int): PBuilder = {
    state match {
      case x: RawEntryState =>
        x.wasCollectionOrMap = true
        state = CollectionState(length, x, underlying.beginCollection(length))
        this
      case _ =>
        // Rule #4 - beginCollection/endCollection must be inside beginEntry/endEntry
        sys.error(s"Unable to call beginCollection() without first calling beginEntry()")
    }
  }
  override def putElement(pickler: (PBuilder) => Unit): PBuilder = {
    state match {
      case x: CollectionState if x.numElements > 0 =>
        // happy case
        state = x.copy(numElements = x.numElements - 1)
        underlying.putElement { nu =>
          val next = new DebugPBuilder(nu)
          pickler(next)
          assert(next.isValidFinalState, s"Failed to correctly write collection element, state: $state")
        }
        this
      case x => sys.error(s"Attempting to write collection in inconsitent state!, state = $x")
    }


  }
  override def endCollection(): Unit =
    state match {
      case x: CollectionState if x.numElements == 0 =>
        state = state.previous
      case x: CollectionState =>
        sys.error(s"Ending a collection before writing the number of specified elements!  expected elements = ${x.numElements}, state = $x")
      case x =>
        sys.error(s"Ending a collection without being the the right state!, state = $x")

    }



  // TODO -theoretically each of these should be returning a new instance of ourselves just in case...
  override def hintKnownSize(knownSize: Int): this.type = {
    underlying.hintKnownSize(knownSize)
    super.hintKnownSize(knownSize)
  }
  override def popHints(): this.type = {
    underlying.popHints()
    super.popHints()
  }
  override def pushHints(): this.type = {
    underlying.pushHints()
    super.pushHints()
  }
  override def hintStaticallyElidedType(): this.type = {
    underlying.hintStaticallyElidedType()
    super.hintStaticallyElidedType()
  }
  override def hintOid(id: Int): this.type = {
    underlying.hintOid(id)
    super.hintOid(id)
  }
  override def pinHints(): this.type = {
    underlying.pinHints()
    super.pinHints()
  }
  override def hintTag(tag: FastTypeTag[_]): this.type = {
    underlying.hintTag(tag)
    super.hintTag(tag)
  }
  override def hintDynamicallyElidedType(): this.type = {
    underlying.hintDynamicallyElidedType()
    super.hintDynamicallyElidedType()
  }
  override def unpinHints(): this.type = {
    underlying.unpinHints()
    super.unpinHints()
  }
}
