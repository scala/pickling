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
               logger.debug(s"Trying algorithm: $next on $tpe")
               next.generate(tpe, logger)
           }
         }
     }
}

/** this algorithm inspects symbols to determine if we have a scala case class, and generates the
  * pickling code for it.
  *
  * This ONLY handles case-class types, it will not handle ADTS.
  *
  * TODO - We want two variants of this.  One which allows runtime java-reflection, and one which does not.  Currently
  *        this allows runtime java reflection for fields/methods.
  */
object CaseClassPickling extends PicklingAlgorithm {
  case class FieldInfo(name: String, sym: IrMethod)
  case class CaseClassInfo(constructor: IrConstructor, fields: Seq[FieldInfo])
  private def checkConstructorImpl(tpe: IrClass, logger: AlgorithmLogger): Option[PickleUnpickleImplementation] = {
    if(tpe.isCaseClass) {
      if(!tpe.isFinal) {
        logger.warn(s"Warning: ${tpe.className} is not final.  Generated unpickling code does not handle subclasses.")
      }

      tpe.primaryConstructor match {
        case Some(c) if c.isPublic =>
          val names = c.parameterNames.flatten.toSet
          val standAloneVars = tpe.methods.filter { m =>
            m.isVar && !(names contains m.methodName)
          }
          // TODO - Allow us to DISABLE serialziing these vars.
          // TODO - Allow us to ERROR on classes like this.
          if(!standAloneVars.isEmpty) {
            logger.warn(s"Warning: ${tpe.className} has a member var not represented in the constructor.  Pickling is not guaranteed to handle this correctly.")
          }

          // Here we need to unify the fields with the constructor names.  We assume they have the same name.
          val fields = for {
            name <- c.parameterNames.flatten.toSeq
            m <- tpe.methods.find(_.methodName == name)
          } yield FieldInfo(name, m)
          if(fields.length == c.parameterNames.flatten.length) {
            val pickle = PickleBehavior(Seq(PickleEntry(fields.map { field =>
              GetField(field.name, field.sym)
            }.toSeq ++ standAloneVars.map { field =>
              GetField(field.methodName, field)
            })))
            val unpickle =
              UnpickleBehavior(
                Seq(CallConstructor(fields.map(_.name), c)) ++
                standAloneVars.map { field =>
                  field.setter match {
                    case Some(mth) => SetField(field.methodName, mth)
                    case _ => sys.error(s"Attempting to define unpickle behavior, when no setter is defined on a var: ${field}")
                  }
                })
            Some(PickleUnpickleImplementation(pickle, unpickle))
          }
          // TODO - what do we do if it doesn't line up?  This is probably some insidious bug.
          else logger.abort(s"Encountered a case class (${tpe.className}) where we could not find all the constructor parameters.")
        case _ =>
          None
      }
    } else None
  }

  def checkFactoryImpl(tpe: IrClass, logger: AlgorithmLogger): Option[PickleUnpickleImplementation] = {
    // THis should be accurate, because all case calsses have companions
    for {
      companion <- tpe.companion
      factoryMethod <-tpe.methods.filter(_.methodName == "apply").sortBy(_.parameterNames.flatten.size).headOption
    } yield {
      val names = factoryMethod.parameterNames.flatten.toSet
      val hasStandaloneVar = tpe.methods.exists { m =>
        m.isVar && !(names contains m.methodName)
      }
      if(hasStandaloneVar) {
        logger.warn(s"Warning: ${tpe.className} has a member var not represented in the constructor.  Pickling is not guaranteed to handle this correctly.")
      }
      val fieldNameList = factoryMethod.parameterNames.flatten.toSeq
      val fields = for {
        name <- fieldNameList
        m <- tpe.methods.find(_.methodName == name)
      } yield FieldInfo(name, m)
      if(fields.length == factoryMethod.parameterNames.flatten.length) {
        val pickle = PickleBehavior(Seq(PickleEntry(fields.map { field =>
          GetField(field.name, field.sym)
        }.toSeq)))
        // TODO - Figure out how to access the module object
        val unpickle = CallModuleFactory(fieldNameList, companion, factoryMethod)
        PickleUnpickleImplementation(pickle, unpickle)
      }
      // TODO - what do we do if it doesn't line up?  This is probably some insidious bug.
      else logger.abort(s"Encountered a case class (${tpe.className}) where we could not find all the constructor parameters.")
    }
  }

  /**
   * Attempts to construct pickling logic fora  given type.
   * @param tpe
   * @return
   */
  override def generate(tpe: IrClass, logger: AlgorithmLogger): Option[PickleUnpickleImplementation] = {
    if(tpe.isCaseClass)
      (checkConstructorImpl(tpe, logger) orElse checkFactoryImpl(tpe, logger))
    else None
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
      case scala.util.Success(Seq()) =>
        logger.warn(s"Failed to create ADT pickler for $tpe.  Type is closed, but could not find subclasses.\n  You can use @directSubclasses to annotate known subclasses.")
        None
      case scala.util.Success(subclasses) =>
        // TODO - Should we check if we need to also serialize our own state, or delegate that to a different algorithm?
        // TODO - Should we allow dynamic dispatch here (reflection)?
        val pickle = PickleBehavior(Seq(SubclassDispatch(subclasses, tpe)))
        val unpickle = UnpickleBehavior(Seq(SubclassUnpicklerDelegation(subclasses, tpe)))
        Some(PickleUnpickleImplementation(pickle, unpickle))
    }
  }
}
// TODO - Scala singleton object serializer
// TODO - Java Serializable Serializer
// TODO - Java Bean serializer
// TODO - Crazy-Avro-like-serializer

