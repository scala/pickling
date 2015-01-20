package scala

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.pickling.pickler.AllPicklers

package object pickling {
  val functions: PickleFunctions = new PickleFunctions {}

  /** Appends the pickle/pickleTo/pickleInto operations onto any type, assuming implicits picklers are available. */
  final class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat, pickler: SPickler[T]): format.PickleType =
      functions.pickle[T](picklee)(format, pickler)
    def pickleInto(builder: PBuilder)(implicit pickler: SPickler[T]): Unit =
      functions.pickleInto(picklee, builder)(pickler)
    // pickleTo remains a macro so further type-checking of [S <:< format.OutputType] occurs,
    // and any implicit conversions required for this.
    def pickleTo[S](output: S)(implicit format: PickleFormat): Unit =
      macro Compat.PickleMacros_pickleTo[T,S]
    //def pickleTo[S](output: S)(implicit format: PickleFormat, pickler: SPickler[T]): Unit =
    //  functions.pickleTo(picklee, output)(pickler, format)
  }

  final class UnpickleOps(val thePickle: Pickle) {
    def unpickle[T](implicit unpickler: Unpickler[T], format: PickleFormat): T =
       // TODO - Ideally we get a compiler error if pickle type doesn't match.
      functions.unpickle(thePickle.asInstanceOf[format.PickleType])(unpickler, format)
  }

  val allPicklers = AllPicklers

  /** Import `scala.pickling.ops._` to introduce `pickle` and `unpickle` methods.
   */
  val ops: Ops = new Ops {}

  /** Import `scala.pickling.Defaults._` to introduce all picklers and ops.
   */
  val Defaults: Ops with AllPicklers  =
    new Ops with AllPicklers {}

  abstract class Ops {
    implicit def toPickleOps[T](picklee: T): PickleOps[T] = new PickleOps(picklee)
    implicit def pickleToUnpickleOps(thePickle: Pickle): UnpickleOps = new UnpickleOps(thePickle)
  }

}
