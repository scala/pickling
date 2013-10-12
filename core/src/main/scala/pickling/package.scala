package scala

import scala.language.experimental.macros

import scala.reflect.runtime.{universe => ru}
import ru._

package object pickling {

  implicit class PickleOps[T](picklee: T) {
    def pickle(implicit format: PickleFormat): format.PickleType = macro Compat.PickleMacros_pickle[T]
    def pickleInto(builder: PBuilder): Unit = macro Compat.PickleMacros_pickleInto[T]
    def pickleTo(output: Output[_])(implicit format: PickleFormat): Unit = macro Compat.PickleMacros_pickleTo[T]
  }
}
