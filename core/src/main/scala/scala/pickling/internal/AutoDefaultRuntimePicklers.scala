package scala.pickling
package internal

import scala.pickling.runtime.{Tuple2RTPickler}
import scala.pickling.spi.PicklerRegistry

/** Helper method to help register picklers which can handle special/crazy classes for runtime generration.
  *
  *
  * Note: Currently this only handles Tuple2s
  */
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
  /** Helper to lookup the very odd, and poorly behaved Tuple2 classes. */
  def lookupTupleSpecialPicklers(classKey: String, tag: FastTypeTag[_]): Option[Pickler[_]] = {
    classKey match {
      case "scala.Tuple2"         => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcII$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcIJ$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcID$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcIC$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcIZ$sp" => Some( new Tuple2RTPickler (tag) )
  
      case "scala.Tuple2$mcJI$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcJJ$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcJD$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcJC$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcJZ$sp" => Some( new Tuple2RTPickler (tag) )
  
      case "scala.Tuple2$mcDI$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcDJ$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcDD$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcDC$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcDZ$sp" => Some( new Tuple2RTPickler (tag) )
  
      case "scala.Tuple2$mcCI$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcCJ$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcCD$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcCC$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcCZ$sp" => Some( new Tuple2RTPickler (tag) )
  
      case "scala.Tuple2$mcZI$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcZJ$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcZD$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcZC$sp" => Some( new Tuple2RTPickler (tag) )
      case "scala.Tuple2$mcZZ$sp" => Some( new Tuple2RTPickler (tag) )
      case _ => None
    }
  } 
}
