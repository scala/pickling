package scala.pickling
package generator

import scala.reflect.api.Universe

// TODO - These need logging messages.

/** An interface so we can pass logging to these algorithms at runtime/during testing. */
trait AlgorithmLogger {
  def warn(msg: String): Unit
  def debug(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
}

/** Represents the result of an algorithm call. */
sealed trait AlgorithmResult {
  def join(other: => AlgorithmResult): AlgorithmResult
  def map(f: PickleUnpickleImplementation => PickleUnpickleImplementation): AlgorithmResult =
     this match {
       case AlgorithmSucccess(i) => AlgorithmSucccess(f(i))
       case x => x
     }
}
final case class AlgorithmSucccess(impl: PickleUnpickleImplementation) extends AlgorithmResult {
  def join(other: => AlgorithmResult): AlgorithmResult = this
}
/** A list of reasons why an algorithm failued to run. */
final case class AlgorithmFailure(reasons: List[String]) extends AlgorithmResult {
  def join(other: => AlgorithmResult): AlgorithmResult =
    other match {
      case x: AlgorithmSucccess => x
      case AlgorithmFailure(rs) => AlgorithmFailure(reasons ++ rs)
    }
}
object AlgorithmFailure {
  def apply(reason: String): AlgorithmFailure = AlgorithmFailure(List(reason))
}
/** An abstract implementation of a pickling generation algorithm.
  *
  *
  * TODO - Do we even need an interfaace?
  */
trait PicklingAlgorithm {
  /**
   * Attempts to construct pickling logic for a given type.
   */
  def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult

  /** Attempts to create pickling logic for a given type.  Not this will automatically issue
    * warnings based on why all algorithms failed, if algorithms do fail.
    */
  def generateImpl(tpe: IrClass, logger: AlgorithmLogger): Option[PickleUnpickleImplementation] = {
    generate(tpe, logger) match {
      case AlgorithmSucccess(success) => Some(success)
      case AlgorithmFailure(failures) =>
        val fString = failures.mkString("\n - ", "\n - ", "\n")
        logger.error(s"Unable to generate pickling/unpickling implementation for $tpe.\n$fString")
        None
    }
  }
}
object PicklingAlgorithm {
  def create(algs: Seq[PicklingAlgorithm]): PicklingAlgorithm =
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

