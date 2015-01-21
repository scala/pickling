package scala.pickling.transienttest

import org.scalatest.FunSuite
import scala.reflect.ClassTag
import scala.pickling._, scala.pickling.Defaults._, json._

case class Person(val name: String , @transient val ssNumber: Int) {
  override def toString = s"Person($name)"
}

class Dependency[T]

class SparkConf(loadDefaults: Boolean)
class SparkContext(config: SparkConf)

class RDD[T: ClassTag](
    @transient private var sc: SparkContext,
    @transient private var deps: Seq[Dependency[_]]
  )

class RangePartitioner[K : ClassTag, V](
    @transient val partitions: Int,
    @transient val rdd: RDD[_ <: Product2[K,V]],
    private var ascending: Boolean = true) {
  override def toString = s"RangePartitioner(ascending = $ascending)"
}

class TransientSimpleTest extends FunSuite {
  test("main") {
    val per = Person("Jenny", 123)
    val p: JSONPickle = per.pickle
    val up = p.unpickle[Person]
    assert(up.ssNumber == 0)
    assert(per.toString == up.toString)
  }
}

class TransientSparkTest extends FunSuite {
  test("main") {
    val sc = new SparkContext(new SparkConf(true))
    val rdd = new RDD[(Int, Int)](sc, Seq(new Dependency()))
    val rp = new RangePartitioner[Int, Int](2, rdd)
    val p: JSONPickle = rp.pickle
    val up = p.unpickle[RangePartitioner[Int, Int]]
    assert(rp.toString == up.toString)
  }
}

