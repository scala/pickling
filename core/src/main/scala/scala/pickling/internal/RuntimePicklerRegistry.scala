package scala.pickling
package internal

import scala.pickling.runtime.CustomRuntime
import scala.pickling.spi.PicklerRegistry

/** Helper class to register picklers and unpicklers that
  * handle special/crazy classes for runtime generation.
  *
  * Related to [[scala.pickling.runtime.CustomRuntime]].
  * Note: Currently this only handles Tuple2s.
  */
trait RuntimePicklerRegistry extends CustomRuntime {
  this: PicklerRegistry =>

  val tupleGenerators = (tuplePicklerGenerator, tupleUnpicklerGenerator)

  /* Don't need to add methods to add/remove from
   * this list because this is meant to be used
   * internally. If a user wants to do the same,
   * she just needs to use the [[PicklerRegistry]]. */
  val picklersToRegister = List(
    ("scala.Tuple2", tupleGenerators),
    ("scala.Tuple2$mcII$sp", tupleGenerators),
    ("scala.Tuple2$mcIJ$sp", tupleGenerators),
    ("scala.Tuple2$mcID$sp", tupleGenerators),
    ("scala.Tuple2$mcIZ$sp", tupleGenerators),

    ("scala.Tuple2$mcJI$sp", tupleGenerators),
    ("scala.Tuple2$mcJJ$sp", tupleGenerators),
    ("scala.Tuple2$mcJD$sp", tupleGenerators),
    ("scala.Tuple2$mcJC$sp", tupleGenerators),
    ("scala.Tuple2$mcJZ$sp", tupleGenerators),

    ("scala.Tuple2$mcDI$sp", tupleGenerators),
    ("scala.Tuple2$mcDJ$sp", tupleGenerators),
    ("scala.Tuple2$mcDD$sp", tupleGenerators),
    ("scala.Tuple2$mcDC$sp", tupleGenerators),
    ("scala.Tuple2$mcDZ$sp", tupleGenerators),

    ("scala.Tuple2$mcCI$sp", tupleGenerators),
    ("scala.Tuple2$mcCJ$sp", tupleGenerators),
    ("scala.Tuple2$mcCD$sp", tupleGenerators),
    ("scala.Tuple2$mcCC$sp", tupleGenerators),
    ("scala.Tuple2$mcCZ$sp", tupleGenerators),

    ("scala.Tuple2$mcZI$sp", tupleGenerators),
    ("scala.Tuple2$mcZJ$sp", tupleGenerators),
    ("scala.Tuple2$mcZD$sp", tupleGenerators),
    ("scala.Tuple2$mcZC$sp", tupleGenerators),
    ("scala.Tuple2$mcZZ$sp", tupleGenerators)
  )

  final def registerRuntimePicklersAtInit(): Unit = {
    for((key, (pickler, unpickler)) <- picklersToRegister) {
      registerPicklerGenerator(key, pickler)
      registerUnpicklerGenerator(key, unpickler)
    }
  }

}
