package scala.pickling
package generator

import scala.reflect.api.Universe

// TODO - These need logging messages.

/** An interface so we can pass logging to these algorithms at runtime/during testing. */
trait AlgorithmLogger {
  def warn(msg: String): Unit
  def debug(msg: String): Unit
  def abort(msg: String): Nothing
}

/** An abstract implementation of a pickling generation algorithm.
  *
  *
  * TODO - Do we even need an interfaace?
  */
trait PicklingAlgorithm {
  /**
   * Attempts to construct pickling logic for a given type.
   *
   * TODO - Instead of Option, these should return an error messages that we can aggregate
   *        to explain why a pickler/unpickler could not be generated for a given type.
   */
  def generate(tpe: IrClass, logger: AlgorithmLogger): Option[PickleUnpickleImplementation]
}
object PicklingAlgorithm {
  def create(algs: Seq[PicklingAlgorithm]): PicklingAlgorithm =
     new PicklingAlgorithm {
       /**
        * Attempts to construct pickling logic for a given type.
        */
       override def generate(tpe: IrClass, logger: AlgorithmLogger): Option[PickleUnpickleImplementation] =
         algs.foldLeft(Option.empty[PickleUnpickleImplementation]) { (prev, next) =>
           prev match {
             case x: Some[_] => x
             case None =>
               logger.debug(s"Trying algorithm: $next")
               next.generate(tpe, logger)
           }
         }
     }
}

/** this algorithm inspects symbols to determine if we have a scala case class, and generates the
  * pickling code for it.
  *
  * This ONLY handles case-class types, it will not handle ADTS.
  */
object CaseClassPickling extends PicklingAlgorithm {
  case class FieldInfo(name: String, sym: IrMethod)
  case class CaseClassInfo(constructor: IrConstructor, fields: Seq[FieldInfo])
  private def getFieldsAndContructor(tpe: IrClass, logger: AlgorithmLogger): Option[CaseClassInfo] = {
    if(tpe.isCaseClass) {
      if(!tpe.isFinal) {
        logger.warn(s"Warning: ${tpe.className} is not final.  Generated unpickling code does not handle subclasses.")
      }
      tpe.primaryConstructor map { c =>
        val names = c.parameterNames.toSet
        val hasStandaloneVar = tpe.methods.exists { m =>
          m.isVar && !(names contains m.methodName)
        }
        if(hasStandaloneVar) {
          logger.warn(s"Warning: ${tpe.className} has a member var not represented in the constructor.  Pickling is not guaranteed to handle this correctly.")
        }

        // Here we need to unify the fields with the constructor names.  We assume they have the same name.
        val fields = for {
          name <- c.parameterNames.toSeq
          m <- tpe.methods.find(_.methodName == name)
        } yield FieldInfo(name, m)
        if(fields.length == c.parameterNames.length) CaseClassInfo(c, fields)
        // TODO - what do we do if it doesn't line up?  This is probably some insidious bug.
        else logger.abort(s"Encountered a case class (${tpe.className}) where we could not find all the constructor parameters.")
      }
    } else None
  }


  /**
   * Attempts to construct pickling logic fora  given type.
   * @param tpe
   * @return
   */
  override def generate(tpe: IrClass, logger: AlgorithmLogger): Option[PickleUnpickleImplementation] = {
    getFieldsAndContructor(tpe, logger) map { structure =>
      val pickle = PickleBehavior(structure.fields.map { field =>
        GetField(field.name, field.sym)
      }.toSeq)
      val unpickle = CallConstructor(structure.fields.map(_.name), structure.constructor)
      PickleUnpickleImplementation(pickle, unpickle)
    }
  }
}

/** This algorithm isnpects symbols to determine if we have an abstract class with fully known sub-classes,
  * in which case we delegate behavior to the subclasses.
  */
object AdtPickling extends PicklingAlgorithm {
  /**
   * Attempts to construct pickling logic for a given type.
   */
  override def generate(tpe: IrClass, logger: AlgorithmLogger): Option[PickleUnpickleImplementation] = {
    tpe.closedSubclasses match {
      case scala.util.Failure(msgs) =>
        // TODO - SHould we warn here, or collect errors for later?
        logger.warn(s"Failed to create ADT pickler = $msgs")
        None
      case scala.util.Success(subclasses) =>
        // TODO - Should we allow dynamic dispatch here?
        val pickle = PickleBehavior(Seq(SubclassDispatch(subclasses, tpe)))
        // TODO - Figure out unpickler
        val unpickle = UnpickleBehavior(Seq())
        Some(PickleUnpickleImplementation(pickle, unpickle))
    }
  }
}

// TODO - Java Serializable Serializer
// TODO - Java Bean serializer
// TODO - Scala singleton object serializer

// TODO - Crazy-Avro-like-serializer

