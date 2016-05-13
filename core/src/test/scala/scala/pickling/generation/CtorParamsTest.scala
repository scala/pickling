package scala.pickling.test.ctorparams

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._, json._

class Partitioner(numParts: Int) {
  def numPartitions = numParts
  override def toString = s"Partitioner($numParts)"
}

class Partitioner2(numParts: Int, val other: String) {
  def numPartitions = numParts
  override def toString = s"Partitioner2($numParts,$other)"
}

class CtorParamsTest extends FunSuite {
  test("runtime pickler") {
    val par: Any = new Partitioner(8)
    val p: JSONPickle = par.pickle
    val up = p.unpickle[Any]
    assert(par.toString == up.toString)
  }

  test("static pickler") {
    val par = new Partitioner(8)
    val p: JSONPickle = par.pickle
    val up = p.unpickle[Partitioner]
    assert(par.toString == up.toString)
  }

  test("ctor param and public getter") {
    val par = new Partitioner2(8, "test")
    val p: JSONPickle = par.pickle
    val up = p.unpickle[Partitioner2]
    assert(par.toString == up.toString)
  }
}
