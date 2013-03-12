package scala.pickling

package object json {
  implicit val pickleFormat = new JSONPickleFormat
  implicit def toJSONPickle(value: String): JSONPickle = new JSONPickle(value)
}

package json {
  import language.experimental.macros
  import scala.collection.immutable.ListMap
  import scala.reflect.api.Universe
  import scala.reflect.runtime.{universe => ru}
  import scala.reflect.macros.Macro
  import scala.util.parsing.json._

  case class JSONPickle(val value: String) extends Pickle {
    type ValueType = String
    type PickleFormatType = JSONPickleFormat
  }

  class JSONPickleFormat extends PickleFormat {
    import ir._
    type PickleType = JSONPickle
    override def instantiate = macro JSONPickleInstantiate.impl
    def formatCT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, accessor: irs.FieldIR => U#Expr[Pickle]): U#Expr[PickleType] = {
      ???
    }
    def formatRT[U <: Universe with Singleton](irs: PickleIRs[U])(cir: irs.ClassIR, accessor: irs.FieldIR => Pickle): PickleType = {
      ???
    }
    def parse(pickle: JSONPickle, mirror: ru.Mirror): Option[UnpickleIR] = {
      ???
    }
  }

  trait JSONPickleInstantiate extends Macro {
    def impl = c.universe.EmptyTree updateAttachment pickleFormat
  }
}
