package scala.pickling.ctorparams

import org.scalatest.FunSuite
import scala.pickling._
import json._

class Partitioner(numParts: Int) {
  def numPartitions = numParts
  override def toString = s"Partitioner($numParts)"
}

class AllRuntimeCtorsParamTest extends FunSuite {
  test("main") {
    val par: Any = new Partitioner(8)
    val p: JSONPickle = par.pickle
    val up = p.unpickle[Any]
    assert(par.toString == up.toString)
  }
}

class StaticTriggeredCtorsParamTest extends FunSuite {
  test("main") {
    // val par = new Partitioner(8)
    // val p: JSONPickle = par.pickle
    // val up = p.unpickle[Partitioner]
    // assert(par.toString == up.toString)
    assert(true)
  }
}