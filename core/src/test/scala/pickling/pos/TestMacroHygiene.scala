package pickling.pos

import scala.pickling.{Unpickler, Pickler, PicklerUnpickler}
import scala.reflect.runtime.universe.WeakTypeTag

final case class HygieneTester(x: Option[Boolean], y: Seq[String])
/**
 * Ensures we have pickling hygiene
 */
class TestMacroHygiene {
  import _root_.scala.pickling.Defaults._
  import _root_.scala.pickling.json._

  // Workaround for scala 2.10 hygiene issues in the WeakTypeTag macros.
  // We need to provide all of these *before* we create hygiene issues, then we can test just the pickling hygiene.
  implicit val wtto = implicitly[WeakTypeTag[Option[Boolean]]]
  implicit val wtts = implicitly[WeakTypeTag[Some[Boolean]]]
  implicit val wttn = implicitly[WeakTypeTag[None.type]]
  implicit val wttf = implicitly[WeakTypeTag[HygieneTester]]
  implicit val wttl = implicitly[WeakTypeTag[Seq[String]]]

  //scala.pickling.Defaults.seqPickler
  // TODO - We should also make sure we can compile List's w/ hygiene, which I think is broken right now.
  def hygiene(): Any = {
    val scala, Any, String, FastTypeTag, Unit = ()
    trait scala; trait Any; trait String; trait FastTypeTag; trait Unit;
    implicit val hgt = PicklerUnpickler.generate[HygieneTester  ]
    HygieneTester(Option(false), Seq("hi")).pickle.unpickle[HygieneTester]
  }
}
