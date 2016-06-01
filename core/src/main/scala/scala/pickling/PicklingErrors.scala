package scala.pickling

import scala.pickling.generator.AlgorithmLogger

private[pickling] object Feedback {

  def failedGenerationMsg(x: String, y: String, z: String) =
    s"Failed generation of a pickler/unpickler for $x, cannot find a $z for $y."

  def failedParsingMsg(x: String, format: String) =
    s"Failed to parse $x as $format."

  def disabledRuntimeGenerationMsg(tpe: String, failed: String) =
    s"Couldn't create $failed for $tpe because runtime generation is disabled."

}

private[pickling] object MacrosErrors {

  import scala.reflect.macros.Context

  def failedGeneration(culprit: String)(implicit c: Context, l: AlgorithmLogger): Nothing = {
    l.error(s"Failed pickler/unpickler generation for $culprit")
    ???
  }

  def impossibleGeneration(culprit: String)(implicit c: Context, l: AlgorithmLogger) =
    l.abort("Scala pickling can't generate pickling logic for $culprit")

}

object PicklingErrors {

  import Feedback._

  /** Super class of any pickling exception that can be thrown. */
  class BasePicklingException(val message: String,
    val cause: Option[Throwable] = None) extends RuntimeException(message, cause.orNull)

  /** Provides `apply` and `extractors` that allow the new change in
    * the errors api while keeping the same public old interface.
    */
  object BasePicklingException {

    def apply(msg: String, cause: Option[Throwable] = None) =
      new BasePicklingException(msg, cause)

    def unapply(b: BasePicklingException): Option[(String, Option[Throwable])] =
      Some(b.message -> b.cause)

  }

  /** Exception thrown when the generation of a [[Pickler]] for a type
    * fails because scala pickling wasn't able to find or generate it.
    *
    * @param tpe General type to be pickled
    * @param concreteTpe Concrete type that caused the error
    * @param cause Exception causing the pickling exception if any
    */
  final case class FailedPicklerGeneration(tpe: String, concreteTpe: String,
    override val cause: Option[Throwable] = None) extends BasePicklingException(
      failedGenerationMsg(tpe, concreteTpe, "pickler"), cause
  )

  /** Exception thrown when the generation of a [[Unpickler]] for a type
    * fails because scala pickling wasn't able to find or generate it.
    *
    * @param tpe General type to be pickled
    * @param concreteTpe Concrete type that caused the error
    * @param cause Exception causing the pickling exception if any
    */
  final case class FailedUnpicklerGeneration(tpe: String, concreteTpe: String,
    override val cause: Option[Throwable] = None) extends BasePicklingException(
      failedGenerationMsg(tpe, concreteTpe, "unpickler"), cause
  )

  /** Exception thrown when it's not possible to generated a [[Pickler]]
    * or [[Unpickler]] at runtime because runtime generation has been disabled.*/
  final case class RuntimeGenerationDisabled(tpe: String, failed: String)
    extends UnsupportedOperationException(
      disabledRuntimeGenerationMsg(tpe, failed)
    )

  /** Represent any error related to parsing. */
  class ParsingException(msg: String) extends BasePicklingException(msg)

  /** Exception thrown when the parsing of a message is not successful.
    *
    * @param value Message that gave the error
    */
  final case class JsonParseFailed(value: String)
    extends ParsingException(failedParsingMsg(value, "json"))

  /** Exception thrown when the parsing of a message is not successful.
    *
    * @param value Message that gave the error
    */
  final case class BinaryParseFailed(value: String)
    extends ParsingException(failedParsingMsg(value, "binary"))

  /** Exception thrown when there is no type hint and the type in the
    * unpickler hasn't been elided.
    */
  case class NoTypeHint(suffix: String) extends ParsingException(
    s"Type is elided in pickle, but no elide hint was provided by the unpickler$suffix")
  object NoTypeHint extends NoTypeHint(".")

  /** Represent any error that violates an assumption made by scala pickling. */
  class LogicException(msg: String) extends BasePicklingException(msg)

  /** Exception thrown to represent any logic error when pickling or unpickling.
    *
    * @param feedbackMsg Feedback on the error
    */
  case class LogicPicklingError(feedbackMsg: String)
    extends LogicException(s"Logic pickling error: $feedbackMsg")

  /** Exception thrown when an expected field hasn't been found.
    * unpickler hasn't been elided.
    *
    * @param name Name of the expected field
    * @param lastReadTag Last read tag in the unpickler
    * @param fields Fields that were read from the message
    */
  final case class FieldNotFound(name: String, lastReadTag: String,
      fields: Map[String, Any]) extends LogicException(
    s"No field '$name' when unpickling, tag $lastReadTag, fields were $fields"
  )

  import scala.language.existentials

  /** Exception thrown to represent a mismatch between [[FastTypeTag]]s.
    *
    * @param expected Expected type tag
    * @param found Received type tags
    */
  final case class TypeMismatch(expected: List[FastTypeTag[_]],
      found: List[FastTypeTag[_]]) extends LogicException(
    s"Error: expected one type argument in $expected, found: $found"
  )

  /** Exception thrown when the unpickling of a message is not successful.
    *
    * @param name Name of the type
    * @param tag Tag in the message to unpickle
    * @param message Error message
    * @param cause Exception causing the pickling exception if any
    */
  final case class FailedUnpickling(name: String, tag: String,
    override val message: String, override val cause: Option[Throwable] = None
  ) extends BasePicklingException(
    s"""Error in unpickle of '$name' (tag in unpickle: '$tag')
        |Message: $message""".stripMargin, cause
  )

  /** Exception thrown when a stream ends unexpectedly during unpickling. */
  object EndOfStreamException extends RuntimeException(
    "End of stream reached while unpickling")

  /** Represent any error that violates an assumption made by scala pickling. */
  class PicklingRuntimeException(msg: String) extends BasePicklingException(msg)

  /** Exception thrown when a pickler is unable to recognize a class.
    *
    * @param clz Java class
    * @param extra Extra information that enriches the feedback
    */
  final case class UnrecognizedClass(clz: Class[_], extra: Option[String])
    extends PicklingRuntimeException(s"Class ${clz.getName} not recognized" +
      s""" by pickler. ${if(extra.isDefined) ", " + extra + "." else "." }"""
    )

  /** Exception thrown when a pickler is unable to recognize a tag.
    *
    * @param tagKey The string representation of a tag
    * @param context Information about where or how has happened
    */
  final case class UnrecognizedTag(tagKey: String, context: String)
    extends PicklingRuntimeException(
      s"Error when $context. Unexpected tag $tagKey could not be recognized."
    )

  /** Used to add some top message to a captured exception, that usually
    * gives some information on the context in which it happened.
    *
    * @param e Wrapped throwable
    * @param msg Information about the context
    * @param delimiter Delimiter between the [[e]] and [[msg]].
    */
  final case class Wrapper(e: Throwable, msg: String, delimiter: String = "\n")
    extends BasePicklingException(s"$msg$delimiter${e.getMessage}", Some(e))

  /** Exception thrown when one tries to unpickle on a Unpickler[Nothing]. */
  case object NothingIsNotUnpicklable extends PicklingRuntimeException(
    s"You called `unpickle` on `Unpickler[Nothing]`, but it cannot be unpickled")

}
