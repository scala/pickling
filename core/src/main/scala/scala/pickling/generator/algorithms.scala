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
   *
   * TODO - Instead of Option, these should return an error messages that we can aggregate
   *        to explain why a pickler/unpickler could not be generated for a given type.
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

/** this algorithm inspects symbols to determine if we have a scala case class, and generates the
  * pickling code for it.
  *
  * This ONLY handles case-class types, it will not handle ADTS.
  *
  * TODO - We want two variants of this.  One which allows runtime java-reflection, and one which does not.  Currently
  *        this allows runtime java reflection for fields/methods.
  */
class CaseClassPickling(val allowReflection: Boolean) extends PicklingAlgorithm {

  case class FieldInfo(name: String, sym: IrMethod)
  case class CaseClassInfo(constructor: IrConstructor, fields: Seq[FieldInfo])
  private def checkConstructorImpl(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    if(tpe.isCaseClass) {
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
            if(!allowReflection && (pickle.requiresReflection || unpickle.requiresReflection)) {
              def reflectionErrorMessage(ast: IrAst): List[String] =
                ast match {
                  case x: SetField if x.requiresReflection => List(s"field ${x.name} has no public setter/accces")
                  case x: GetField if x.requiresReflection => List(s"field ${x.name} has no public getter/access")
                  case x: CallConstructor if x.requiresReflection => List(s"constructor is not public")
                  case x: CallModuleFactory if x.requiresReflection => List(s"factory method (apply) is not public")
                  case x: PickleEntry => x.ops.toList.flatMap(reflectionErrorMessage)
                  case x: PickleBehavior => x.operations.toList.flatMap(reflectionErrorMessage)
                  case x: UnpickleBehavior => x.operations.toList.flatMap(reflectionErrorMessage)
                  // TODO - other instances we need to delegate?
                  case _ => List()
                }
              val errors = (reflectionErrorMessage(pickle) ++ reflectionErrorMessage(unpickle))
              val errorString = if(errors.isEmpty) "   unknown reason" else errors.mkString("   - ", "\n   - ", "")
              AlgorithmFailure(s"Cannot pickle case class, because reflection is not allowed and some members are protected/private.\n$errorString")
            } else AlgorithmSucccess(PickleUnpickleImplementation(pickle, unpickle))
          }
          // TODO - what do we do if it doesn't line up?  This is probably some insidious bug.
          else logger.abort(s"Encountered a case class (${tpe.className}) where we could not find all the constructor parameters.")
        case _ =>
          AlgorithmFailure("case-class constructor is not public")
      }
    } else AlgorithmFailure("class is not a case class")
  }

  def checkFactoryImpl(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    // THis should be accurate, because all case calsses have companions
    (for {
      companion <- tpe.companion
      factoryMethod <-tpe.methods.filter(_.methodName == "apply").sortBy(_.parameterNames.flatten.size).headOption
    } yield {
      val names = factoryMethod.parameterNames.flatten.toSet
      val hasStandaloneVar = tpe.methods.exists { m =>
        m.isVar && !(names contains m.methodName)
      }
      // TODO - This should lead to a failure IF we're in no-reflection mode.
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
    }) match {
      case Some(success) => AlgorithmSucccess(success)
      case None => AlgorithmFailure("Could not find a valid case-class factory method.")
    }
  }

  /**
   * Attempts to construct pickling logic fora  given type.
   * @param tpe
   * @return
   */
  override def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    if(tpe.isCaseClass) {
      // TODO - Make this fatal or handle it...  If we want to handle it, we need to wrap our implementations for the
      //        case class with "check for subclass" guards.  We should also check to see if we *know* the
      //        possible subclasses or not.
      if(!tpe.isFinal) {
        logger.warn(s"Warning: ${tpe.className} is not final.  Generated unpickling code does not handle subclasses.")
      }
      (checkConstructorImpl(tpe, logger) join checkFactoryImpl(tpe, logger))
    } else AlgorithmFailure(s"Cannot use case-class algorithm on non-case class $tpe")
  }
}

/** This algorithm isnpects symbols to determine if we have an abstract class with fully known sub-classes,
  * in which case we delegate behavior to the subclasses.
  */
object AdtPickling extends PicklingAlgorithm {
  /**
   * Attempts to construct pickling logic for a given type.
   */
  override def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    if(!tpe.isAbstract) {
      AlgorithmFailure(s"Cannot use ADT algorithm because $tpe is not abstract")
    } else tpe.closedSubclasses match {
      case scala.util.Failure(msgs) =>
        // TODO - SHould we warn here, or collect errors for later?
        AlgorithmFailure(s"Could not determine if $tpe is closed for ADT generation:\n\t\t$msgs")
      case scala.util.Success(Seq()) =>
        AlgorithmFailure(s"Failed to create ADT pickler for $tpe.  Type is closed, but could not find subclasses.\n  You can use @directSubclasses to annotate known subclasses.")
      case scala.util.Success(subclasses) =>
        // TODO - Should we check if we need to also serialize our own state, or delegate that to a different algorithm?
        // TODO - Should we allow dynamic dispatch here (reflection)?
        val pickle = PickleBehavior(Seq(SubclassDispatch(subclasses, tpe)))
        val unpickle = UnpickleBehavior(Seq(SubclassUnpicklerDelegation(subclasses, tpe)))
        AlgorithmSucccess(PickleUnpickleImplementation(pickle, unpickle))
    }
  }
}
// TODO - Scala singleton object serializer
// TODO - Java Serializable Serializer
// TODO - Java Bean serializer
// TODO - Crazy-Kryo-like-serializer

