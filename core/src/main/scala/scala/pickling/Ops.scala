package scala.pickling

import scala.language.experimental.macros
import scala.language.implicitConversions

/** Appends the pickle/pickleTo/pickleInto operations onto any type, assuming implicits picklers are available. */
final case class PickleOps[T](picklee: T) {
  def pickle(implicit format: PickleFormat, pickler: Pickler[T]): format.PickleType =
    functions.pickle[T](picklee)(format, pickler)
  def pickleInto(builder: PBuilder)(implicit pickler: Pickler[T]): Unit =
    functions.pickleInto(picklee, builder)(pickler)
  // pickleTo remains a macro so further type-checking of [S <:< format.OutputType] occurs,
  // and any implicit conversions required for this.
  def pickleTo[S](output: S)(implicit format: PickleFormat): Unit =
    macro Compat.PickleMacros_pickleTo[T,S]
  //def pickleTo[S](output: S)(implicit format: PickleFormat, pickler: Pickler[T]): Unit =
  //  functions.pickleTo(picklee, output)(pickler, format)
}

final case class UnpickleOps(val thePickle: Pickle) {
  def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T =
     // TODO - Ideally we get a compiler error if pickle type doesn't match.
    functions.unpickle(thePickle.asInstanceOf[format.PickleType])(unpickler, format)
}

/** A layer of protocol stack that can "lift" picklable types to have the core pickle operations.
  *
  * For example usage, see [[scala.pickling.Defaults]]
  */
trait Ops {
  implicit def pickleOps[T](picklee: T): PickleOps[T] = PickleOps(picklee)
  implicit def unpickleOps(thePickle: Pickle): UnpickleOps = UnpickleOps(thePickle)
}
