package scala.pickling.transienttest2

import org.scalatest.FunSuite

import scala.reflect.ClassTag
import scala.pickling._
import json._

/**
 * Base class for dependencies.
 */
/*abstract*/ class Dependency[T](val rdd: RDD[T]) //extends Serializable


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


//class Dependency[T]

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
}

class FlatMappedRDD[U: ClassTag, T: ClassTag](
    prev: RDD[T],
    f: T => TraversableOnce[U])
  extends RDD[U](prev) {

  //override def getPartitions: Array[Partition] = firstParent[T].partitions

  //override def compute(split: Partition, context: TaskContext) =
    //firstParent[T].iterator(split, context).flatMap(f)
}


class Transient2SparkTest extends FunSuite {
  test("main") {
    val sc = new SparkContext(new SparkConf(true))
    val rdd = new RDD[Int](sc, Seq(new Dependency(null)))
    val fmrdd = new FlatMappedRDD[Int, Int](rdd, (x: Int) => (1 to x).toList)

    //val rp = new RangePartitioner[Int, Int](2, rdd)
    val p: JSONPickle = fmrdd.pickle
    //val up = p.unpickle[FlatMappedRDD[Int, Int]]
    //assert(fmrdd.toString == up.toString)
  }
}


/*
package org.apache.spark

import org.apache.log4j.{LogManager, PropertyConfigurator}
import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.impl.StaticLoggerBinder

/**
 * Utility trait for classes that want to log data. Creates a SLF4J logger for the class and allows
 * logging messages at different levels using methods that only evaluate parameters lazily if the
 * log level is enabled.
 */
trait Logging {
  // Make the log field transient so that objects with Logging can
  // be serialized and used on another machine
  @transient private var log_ : Logger = null

  // Method to get or create the logger for this object
  protected def log: Logger = {
    if (log_ == null) {
      initializeIfNecessary()
      var className = this.getClass.getName
      // Ignore trailing $'s in the class names for Scala objects
      if (className.endsWith("$")) {
        className = className.substring(0, className.length - 1)
      }
      log_ = LoggerFactory.getLogger(className)
    }
    log_
  }

  // Log methods that take only a String
  protected def logInfo(msg: => String) {
    if (log.isInfoEnabled) log.info(msg)
  }

  protected def logDebug(msg: => String) {
    if (log.isDebugEnabled) log.debug(msg)
  }

  protected def logTrace(msg: => String) {
    if (log.isTraceEnabled) log.trace(msg)
  }

  protected def logWarning(msg: => String) {
    if (log.isWarnEnabled) log.warn(msg)
  }

  protected def logError(msg: => String) {
    if (log.isErrorEnabled) log.error(msg)
  }

  // Log methods that take Throwables (Exceptions/Errors) too
  protected def logInfo(msg: => String, throwable: Throwable) {
    if (log.isInfoEnabled) log.info(msg, throwable)
  }

  protected def logDebug(msg: => String, throwable: Throwable) {
    if (log.isDebugEnabled) log.debug(msg, throwable)
  }

  protected def logTrace(msg: => String, throwable: Throwable) {
    if (log.isTraceEnabled) log.trace(msg, throwable)
  }

  protected def logWarning(msg: => String, throwable: Throwable) {
    if (log.isWarnEnabled) log.warn(msg, throwable)
  }

  protected def logError(msg: => String, throwable: Throwable) {
    if (log.isErrorEnabled) log.error(msg, throwable)
  }

  protected def isTraceEnabled(): Boolean = {
    log.isTraceEnabled
  }

  private def initializeIfNecessary() {
    if (!Logging.initialized) {
      Logging.initLock.synchronized {
        if (!Logging.initialized) {
          initializeLogging()
        }
      }
    }
  }

  private def initializeLogging() {
    // If Log4j is being used, but is not initialized, load a default properties file
    val binder = StaticLoggerBinder.getSingleton
    val usingLog4j = binder.getLoggerFactoryClassStr.endsWith("Log4jLoggerFactory")
    val log4jInitialized = LogManager.getRootLogger.getAllAppenders.hasMoreElements
    if (!log4jInitialized && usingLog4j) {
      val defaultLogProps = "org/apache/spark/log4j-defaults.properties"
      val classLoader = this.getClass.getClassLoader
      Option(classLoader.getResource(defaultLogProps)) match {
        case Some(url) =>
          PropertyConfigurator.configure(url)
          log.info(s"Using Spark's default log4j profile: $defaultLogProps")
        case None =>
          System.err.println(s"Spark was unable to load $defaultLogProps")
      }
    }
    Logging.initialized = true

    // Force a call into slf4j to initialize it. Avoids this happening from mutliple threads
    // and triggering this: http://mailman.qos.ch/pipermail/slf4j-dev/2010-April/002956.html
    log
  }
}

private object Logging {
  @volatile private var initialized = false
  val initLock = new Object()
}
*/