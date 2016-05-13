package scala.pickling.test.issue229

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class Dim(width: Double, height: Double)

class Issue229 extends FunSuite {

  test("Issue #229") {
    val hist: Map[Dim, Int] = List((Dim(0.5d, 0.3d), 5), (Dim(1.5d, 1.3d), 15)).toMap

    val serMap: Map[(Double, Double), Int] =
      hist.map(p => (p._1.width, p._1.height) -> p._2)

    val p = serMap.pickle
    val up = p.unpickle[Map[(Double, Double), Int]]

    assert(up == serMap)
  }

}
