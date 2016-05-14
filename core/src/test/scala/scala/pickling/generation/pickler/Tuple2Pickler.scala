package scala.pickling.generation.pickler

import org.scalatest.FunSuite

import scala.pickling._
import Defaults._
import json._
import scala.pickling.util.ClassMapper
import static._

class Tuple2Pickler extends FunSuite {

  test("pickle/unpickle (Int, String)") {
    val t = (1, "")
    val t2 = t.pickle.unpickle[(Int, String)]
    assert(t === t2)
  }

  test("pickle/unpickle any specialized tuple like (Int, Int)") {
    val t = (1, 2)
    val clz = classOf[(Int, Int)]
    val clzT = t.getClass
    println(ClassMapper.areSameClasses(clzT, clz))
    val t2 = t.pickle.unpickle[(Int, Int)]
    assert(t === t2)
  }

}
