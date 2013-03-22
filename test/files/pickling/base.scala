import scala.pickling._
import json._

sealed abstract class Base
final class C extends Base { override def toString = "C" }
final class D extends Base { override def toString = "D" }

object Test extends App {
  val c: Base = new C
  val pc = c.pickle
  println(pc.unpickle[Base])

  val d: Base = new D
  val pd = d.pickle
  println(pd.unpickle[Base])
}