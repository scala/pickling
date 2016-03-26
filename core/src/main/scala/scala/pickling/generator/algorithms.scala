package scala.pickling
package generator

import scala.reflect.api.Universe

/** An interface so we can pass logging to these algorithms at runtime/during testing. */
private[pickling] trait AlgorithmLogger {
  def warn(msg: String): Unit
  def debug(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
}

/** Represents the result of an algorithm call. */
private[pickling]sealed trait AlgorithmResult {
  def join(other: => AlgorithmResult): AlgorithmResult
  def map(f: PickleUnpickleImplementation => PickleUnpickleImplementation): AlgorithmResult =
     this match {
       case AlgorithmSucccess(i) => AlgorithmSucccess(f(i))
       case x => x
     }
}
private[pickling]final case class AlgorithmSucccess(impl: PickleUnpickleImplementation) extends AlgorithmResult {
  def join(other: => AlgorithmResult): AlgorithmResult = this
}
/** A list of reasons why an algorithm failued to run. */
private[pickling]final case class AlgorithmFailure(reasons: List[String]) extends AlgorithmResult {
  def join(other: => AlgorithmResult): AlgorithmResult =
    other match {
      case x: AlgorithmSucccess => x
      case AlgorithmFailure(rs) => AlgorithmFailure(reasons ++ rs)
    }
}
private[pickling]object AlgorithmFailure {
  def apply(reason: String): AlgorithmFailure = AlgorithmFailure(List(reason))
}
/** An algorithm whcih can generate the IR of a pickler/unpickler *OR* a  good set of error messages. */
private[pickling] trait PicklingAlgorithm {
  /**
   * Attempts to construct pickling logic for a given type.
   */
  def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult
}
private[pickling] object PicklingAlgorithm {
  def run(alg: PicklingAlgorithm)(tpe: IrClass, logger: AlgorithmLogger): Option[PickleUnpickleImplementation] = {
    alg.generate(tpe, logger) match {
      case AlgorithmSucccess(success) => Some(success)
      case AlgorithmFailure(failures) =>
        val fString = failures.mkString("\n - ", "\n - ", "\n")
        logger.error(s"Unable to generate pickling/unpickling implementation for $tpe.\n$fString")
        None
    }
  }


  /** Aggregates the sequence of picklers (prioritizing left-to-right or first-to-last) to
    * create a new "uber" pickling algorithm.
    * @param algs
    *             The list of pickling algorithms to try, in-order.
    * @return
    *         A new algorithm which will try all passed in algorithms and only fail if all algorithms fail.
    */
  def aggregate(algs: Seq[PicklingAlgorithm]): PicklingAlgorithm =
     new PicklingAlgorithm {
       /**
        * Attempts to construct pickling logic for a given type.
        */
       override def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult =
         algs.foldLeft(AlgorithmFailure(List()): AlgorithmResult) { (prev, next) =>
           prev match {
             case x: AlgorithmSucccess => x
             case y: AlgorithmFailure =>
               //logger.debug(s"Trying algorithm: $next on $tpe")
               y join next.generate(tpe, logger)
           }
         }
     }
}


// TODO - Java Serializable Serializer
// TODO - Java Bean serializer
// TODO - Crazy-Kryo-like-serializer

