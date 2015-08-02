package scala.pickling
package internal

import scala.pickling.runtime.{GlobalRegistry, Tuple2RTPickler}
import scala.pickling.spi.PicklerRegistry

/** Helper method to help register picklers which can handle special/crazy classes for runtime generration. */
trait RuntimePicklerRegistryHelper extends PicklerRegistry {
  final def autoRegisterDefaults(): Unit = {
    registerUnpickler("scala.Tuple2", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcII$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcIJ$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcID$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcIC$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcIZ$sp", (new Tuple2RTPickler(null)))

    registerUnpickler("scala.Tuple2$mcJI$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcJJ$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcJD$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcJC$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcJZ$sp", (new Tuple2RTPickler(null)))

    registerUnpickler("scala.Tuple2$mcDI$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcDJ$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcDD$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcDC$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcDZ$sp", (new Tuple2RTPickler(null)))

    registerUnpickler("scala.Tuple2$mcCI$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcCJ$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcCD$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcCC$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcCZ$sp", (new Tuple2RTPickler(null)))

    registerUnpickler("scala.Tuple2$mcZI$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcZJ$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcZD$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcZC$sp", (new Tuple2RTPickler(null)))
    registerUnpickler("scala.Tuple2$mcZZ$sp", (new Tuple2RTPickler(null)))
  }
}
