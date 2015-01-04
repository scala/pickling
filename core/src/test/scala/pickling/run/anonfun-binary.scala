package scala.pickling.run.anonfun

import org.scalatest.FunSuite
import scala.pickling._
import binary._
import AllPicklers._

class AnonfunBinaryTest extends FunSuite {
  val fun: Int => Int = (x: Int) => x + 1

  test("main") {
    val p = fun.pickle
    val up = p.unpickle[Int => Int]
    assert(up(5) === 6)
  }
}
