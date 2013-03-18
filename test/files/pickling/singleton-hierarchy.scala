import scala.pickling._
import json._

sealed abstract class C(val x: Int)
object D extends C(42) { override def toString = "D" }
case class E(override val x: Int) extends C(x)

object Test extends App {
  def test(c: C): Unit = {
    println(c)
    val pickle = c.pickle
    println(pickle)
    println(pickle.unpickle[C])
  }
  test(D)
  test(E(2))
}
