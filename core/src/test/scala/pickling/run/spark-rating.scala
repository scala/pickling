package scala.pickling.transienttest

import org.scalatest.FunSuite
import scala.reflect.ClassTag
import scala.pickling._, Defaults._

// class Dependency[T]
// class SparkContext(config: SparkConf)
// class RDD[T: ClassTag](
//     @transient private var sc: SparkContext,
//     @transient private var deps: Seq[Dependency[_]]
//   )

case class Rating(val user: Int, val product: Int, val rating: Double)

class SparkRatingTest extends FunSuite {
  test("json") {
    import json._
    val r = Rating(12, 12, 12.0)
    val p: JSONPickle = r.pickle
    val up = p.unpickle[Rating]
    assert(up.user == 12)
    assert(up.product == 12)
    assert(up.product == 12.0)
  }

  test("binary") {
    import binary._
    val r = Rating(12, 12, 12.0)
    val p: BinaryPickle = r.pickle
    val up = p.unpickle[Rating]
    assert(up.user == 12)
    assert(up.product == 12)
    assert(up.product == 12.0)
  }
}

class SparkRuntimeRatingTest extends FunSuite {
  test("json") {
    import json._
    val r: Any = Rating(12, 12, 12.0)
    val p: JSONPickle = r.pickle
    val up = p.unpickle[Any].asInstanceOf[Rating]
    assert(up.user == 12)
    assert(up.product == 12)
    assert(up.product == 12.0)
  }

  test("binary") {
    import binary._
    val r: Any = Rating(12, 12, 12.0)
    val p: BinaryPickle = r.pickle
    val up = p.unpickle[Any].asInstanceOf[Rating]
    assert(up.user == 12)
    assert(up.product == 12)
    assert(up.product == 12.0)
  }
}