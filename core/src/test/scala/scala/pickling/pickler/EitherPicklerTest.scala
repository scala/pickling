package scala.pickling.pickler

import org.scalatest.FunSuite

/**
 * Tests Either picklers
 */
class EitherPicklerTest extends FunSuite {
  import scala.pickling._, Defaults._, static._, json._
  test("pickle Left") {
    val l: Left[Int, String] = Left(1)
    val up = l.pickle.unpickle[Left[Int,String]]
    assert(l == up)

    val l2: Either[Int, String] = Left(2)
    val up2 = l2.pickle.unpickle[Left[Int,String]]
    assert(l2 == up2)
    val up22 = l2.pickle.unpickle[Either[Int,String]]
    assert(l2 == up22)
  }

  test("pickle Right") {
    val r: Right[Int, String] = Right("hi")
    val up = r.pickle.unpickle[Right[Int,String]]
    assert(r == up)

    val r2: Either[Int, String] = Right("hello")
    val up2 = r2.pickle.unpickle[Right[Int,String]]
    assert(r2 == up2)
    val up22 = r2.pickle.unpickle[Either[Int,String]]
    assert(r2 == up22)
  }
}
