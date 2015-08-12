package scala.pickling.transienttest2

import org.scalatest.FunSuite

import scala.reflect.ClassTag
import scala.pickling._, scala.pickling.Defaults._, json._

/**
 * Base class for dependencies.
 */
class Dependency[T](val rdd: RDD[T]) //extends Serializable


/**
 * Base class for dependencies where each partition of the parent RDD is used by at most one
 * partition of the child RDD.  Narrow dependencies allow for pipelined execution.
 */
abstract class NarrowDependency[T](rdd: RDD[T]) extends Dependency(rdd) {
  /**
   * Get the parent partitions for a child partition.
   * @param partitionId a partition of the child RDD
   * @return the partitions of the parent RDD that the child partition depends upon
   */
  def getParents(partitionId: Int): Seq[Int]
}


/**
 * Represents a one-to-one dependency between partitions of the parent and child RDDs.
 */
class OneToOneDependency[T](rdd: RDD[T]) extends NarrowDependency[T](rdd) {
  override def getParents(partitionId: Int) = List(partitionId)
}

class SparkConf(loadDefaults: Boolean)
class SparkContext(config: SparkConf)

class RDD[T: ClassTag](
    @transient private var sc: SparkContext,
    @transient private var deps: Seq[Dependency[_]]
  ) /*extends Serializable with Logging*/ {

  /** Construct an RDD with just a one-to-one dependency on one parent */
  def this(@transient oneParent: RDD[_]) =
    this(oneParent.context , List(new OneToOneDependency(oneParent)))

  def context = sc

  var x = 5

  override def toString = s"RDD($x)"
}

class FlatMappedRDD[U: ClassTag, T: ClassTag](
    val prev: RDD[T],
    f: T => TraversableOnce[U])
  extends RDD[U](prev) {

  private var fun = f

  def getIt = fun

  //override def getPartitions: Array[Partition] = firstParent[T].partitions

  //override def compute(split: Partition, context: TaskContext) =
    //firstParent[T].iterator(split, context).flatMap(f)

  override def toString = s"FlatMapped($prev, $f, $x)"

}


class Transient2SparkTest extends FunSuite {
  test("main") {
    val sc = new SparkContext(new SparkConf(true))
    val rdd = new RDD[Int](sc, Seq(new Dependency(null)))
    // TODo - There's a bug with implicit expansion of Seq picklers, which we need to fix.
    //implicit def depP = PicklerUnpickler.generate[Dependency[_]]
    //implicit val seqDepp = Defaults.seqPickler[Dependency[_]]
    //implicit val rddpp = PicklerUnpickler.generate[RDD[Int]]
    //implicit val fmrddp = PicklerUnpickler.generate[FlatMappedRDD[Int, Int]]
    val fmrdd = new FlatMappedRDD[Int, Int](rdd, (x: Int) => (1 to x).toList)
    val p: JSONPickle =
      fmrdd.pickle
    //System.err.println(p)
    val up = p.unpickle[FlatMappedRDD[Int, Int]]
    //System.err.println(up)
    //System.err.println(s"up.x = ${up.x}, up.getIt = ${up.getIt}")

    assert(up.getIt(5) == List(1, 2, 3, 4, 5))
    assert(up.getIt(up.x) == List(1, 2, 3, 4, 5))
  }
}
