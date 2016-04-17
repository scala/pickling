package scala.pickling
package internal

import scala.pickling.pickler.AnyPicklerUnpickler
import scala.pickling.runtime.{Tuple2RTKnownTagUnpickler, Tuple2RTPickler}
import scala.pickling.spi.PicklerRegistry

/** Helper method to help register picklers which can handle special/crazy classes for runtime generration.
  *
  *
  * Note: Currently this only handles Tuple2s
  */
trait RuntimePicklerRegistryHelper extends PicklerRegistry {
  final def autoRegisterDefaults(): Unit = {
    val tpes = List(
      "scala.Tuple2",
      "scala.Tuple2$mcII$sp",
      "scala.Tuple2$mcIJ$sp",
      "scala.Tuple2$mcID$sp",
      "scala.Tuple2$mcIZ$sp",

      "scala.Tuple2$mcJI$sp",
      "scala.Tuple2$mcJJ$sp",
      "scala.Tuple2$mcJD$sp",
      "scala.Tuple2$mcJC$sp",
      "scala.Tuple2$mcJZ$sp",

      "scala.Tuple2$mcDI$sp",
      "scala.Tuple2$mcDJ$sp",
      "scala.Tuple2$mcDD$sp",
      "scala.Tuple2$mcDC$sp",
      "scala.Tuple2$mcDZ$sp",

      "scala.Tuple2$mcCI$sp",
      "scala.Tuple2$mcCJ$sp",
      "scala.Tuple2$mcCD$sp",
      "scala.Tuple2$mcCC$sp",
      "scala.Tuple2$mcCZ$sp",

      "scala.Tuple2$mcZI$sp",
      "scala.Tuple2$mcZJ$sp",
      "scala.Tuple2$mcZD$sp",
      "scala.Tuple2$mcZC$sp",
      "scala.Tuple2$mcZZ$sp"
    )

    for(tpe <- tpes) {
      registerPicklerGenerator(tpe, tuplePicklerGenerator)
      registerUnpicklerGenerator(tpe, tupleUnpicklerGenerator)
    }
  }

  def tuplePicklerGenerator: FastTypeTag[_] => Pickler[(Any,Any)] with Unpickler[(Any, Any)] = { tpe =>
    // TODO - Actually extract the tpe of the internal things.
    val tag = FastTypeTag.apply(tpe.toString)
    new Tuple2RTPickler()
  }

  def tupleUnpicklerGenerator: FastTypeTag[_] => Unpickler[(Any,Any)] = {
    case FastTypeTag(_, List(left, right)) =>
      val lhs = 
        currentRuntime.picklers.lookupUnpickler(left.toString).getOrElse(AnyPicklerUnpickler).asInstanceOf[Unpickler[Any]]
      val rhs =
        currentRuntime.picklers.lookupUnpickler(right.toString).getOrElse(AnyPicklerUnpickler).asInstanceOf[Unpickler[Any]]
      new Tuple2RTKnownTagUnpickler(lhs, rhs)
    case tpe => new Tuple2RTPickler()
  }
}
