package scala.pickling.fastbinary
package test

import org.scalatest.FunSuite

class FastArrayOutputTest extends FunSuite {

  test("target() overflow") {
    val out = new FastArrayOutput
    // fill up cached array
    for (_ <- 1 to 64 * 1024 * 1024 - 1) {
      out += 5
    }
    val out2 = new FastArrayOutput // does checkpoint()
    val (a, pos) = out2.target(2) // (Array[Byte], Int)
    a(pos) = 6
    out2.flush(a)
    val res = out2.result()
    assert(res.length == 2)
    assert(res(0) == 6)
  }

}
