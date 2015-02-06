package scala.pickling.singleton.hierarchy

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

sealed abstract class C(val x: Int)
object D extends C(42) { override def toString = "D" }
case class E(override val x: Int) extends C(x)

class SingletonHierarchyTest extends FunSuite {
  test("main") {
    def test(c: C, expected: String): Unit = {
      val pickle = c.pickle
      assert(pickle.toString === expected)
      assert(pickle.unpickle[C] === c)
    }
    test(D, """
      |JSONPickle({
      |  "$type": "scala.pickling.singleton.hierarchy.D.type"
      |})
    """.stripMargin.trim)
    test(E(2), """
      |JSONPickle({
      |  "$type": "scala.pickling.singleton.hierarchy.E",
      |  "x": 2
      |})
    """.stripMargin.trim)
  }
}
