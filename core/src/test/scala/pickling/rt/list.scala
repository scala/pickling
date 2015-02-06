package scala.pickling
package test.rt

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

case class Person(age: Int)

class RuntimeListAnyRefSuite extends FunSuite {

  test("case class: non-empty") {
    val lst: List[Person] = List(Person(20), Person(40))
    val p = (lst: Any).pickle
    val up = p.unpickle[Any]
    assert(lst == up)
  }

  test("case class: empty") {
    val lst: List[Person] = List()
    val p = (lst: Any).pickle
    val up = p.unpickle[Any]
    assert(lst == up)
  }

  test("tuple: non-empty") {
    val lst: List[(Int, String)] = List((4, "a"), (5, "b"), (6, "c"))
    val p = (lst: Any).pickle
    val up = p.unpickle[Any]
    assert(lst == up)
  }

  test("tuple: empty") {
    val lst: List[(Int, String)] = List()
    val p = (lst: Any).pickle
    val up = p.unpickle[Any]
    assert(lst == up)
  }

  test("tuple with array") {
    val lst: List[(Int, Array[Double])] = List((4, Array.ofDim[Double](10)), (5, Array.ofDim[Double](10)), (6, Array.ofDim[Double](10)))
    val p = (lst: Any).pickle
    val up = p.unpickle[Any]
    val upl = up.asInstanceOf[List[(Int, Array[Double])]]
    assert(lst.map(_._1) == upl.map(_._1))
  }

}
