package scala.pickling
package generator


/** this algorithm inspects symbols to determine if we have a scala case class, and generates the
  * pickling code for it.
  *
  * This ONLY handles case-class types, it will not handle ADTS, but can generate code for non-final case classes.
  *
  */
class CaseClassPickling(val allowReflection: Boolean, val careAboutSubclasses: Boolean) extends PicklingAlgorithm {
  case class FieldInfo(name: String, sym: IrMethod)
  case class CaseClassInfo(constructor: IrConstructor, fields: Seq[FieldInfo])


  // TODO - This helper method should be available elsewhere.
  def allVars(cls: IrClass): Seq[IrMethod] = {
    ( cls.methods.filter(_.isParamAccessor) ++
      IrSymbol.allDeclaredMethodIncludingSubclasses(cls).filter(x => x.isVar || x.isVal)
    ).groupBy(_.methodName).map(_._2.head).toList.filterNot(_.isMarkedTransient)
  }
  private def checkConstructorImpl(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    if(tpe.isCaseClass) {
      tpe.primaryConstructor match {
        case Some(c) if c.isPublic =>
          val names = c.parameterNames.flatten.toSet
          val vars = allVars(tpe)
          val standAloneVars = vars.filter { m =>
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
            // NOTE: here we use the vars list, because it's already filtered out transient vars.
            m <- vars.find(_.methodName == name)
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
          else AlgorithmFailure(s"Encountered a case class (${tpe.className}) where we could not find all the constructor parameters.  This may be because some fields are marked transient.")
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
        val vars = allVars(tpe)
        val names = factoryMethod.parameterNames.flatten.toSet
        val hasStandaloneVar = vars.exists { m =>
          m.isVar && !(names contains m.methodName)
        }
        // TODO - We should have this be configured to be a failure or silenced.  Also, we should copy the var options from above.
        if(hasStandaloneVar) {
          logger.warn(s"Warning: ${tpe.className} has a member var not represented in the constructor.  Pickling is not guaranteed to handle this correctly.")
        }
        val fieldNameList = factoryMethod.parameterNames.flatten.toSeq
        val fields = for {
          name <- fieldNameList
          m <- vars.find(_.methodName == name)
        } yield FieldInfo(name, m)
        if(fields.length == factoryMethod.parameterNames.flatten.length) {
          val pickle = PickleBehavior(Seq(PickleEntry(fields.map { field =>
            GetField(field.name, field.sym)
          }.toSeq)))
          val unpickle = UnpickleBehavior(Seq(CallModuleFactory(fieldNameList, companion, factoryMethod)))
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
    // Scala modules are pickled differently, so we have to explicitly ignore `case object`
    if(tpe.isCaseClass && !tpe.isScalaModule) {
      val behavior = (checkConstructorImpl(tpe, logger) join checkFactoryImpl(tpe, logger))
      if(careAboutSubclasses && !tpe.isFinal) {
        // TODO - We need a different flag to say if we'll use runtime picklers *VS* reflection. The two features are not the same.
        tpe.closedSubclasses match {
          case scala.util.Success(subs) =>
            assert(subs.exists(_.className == tpe.className))
            behavior map { b =>
              IrAst.transform(b) {
                case x: PickleEntry =>
                  SubclassDispatch.apply(
                    subClasses = subs.filterNot(_.className == tpe.className),
                    tpe,
                    Some(x),
                    allowReflection // TODO - This should be `allow runtime pickler lookup`.
                  )
                case x: UnpickleBehavior => UnpickleBehavior(Seq(SubclassUnpicklerDelegation(Nil, tpe, Some(x), allowReflection))) // TODO - This should be `allow runtime pickler lookup`.
                case x => x
              }.asInstanceOf[PickleUnpickleImplementation]
            }
          case scala.util.Failure(ex) if !allowReflection =>
            AlgorithmFailure(s"Case class $tpe is not final, and all subclasses are unknown.  Annotate with @directSubclasses or mark final.")
          case scala.util.Failure(ex) =>
            behavior.map { b =>
              logger.warn(s"Warning:   Unpickler does not currently handle subclasss dipatch for type: $tpe")
              IrAst.transform(b) {
                case x: PickleEntry => SubclassDispatch(Nil, tpe, Some(x), allowReflection) // TODO - This should be `allow runtime pickler lookup`.
                case x: UnpickleBehavior => UnpickleBehavior(Seq(SubclassUnpicklerDelegation(Nil, tpe, Some(x), allowReflection))) // TODO - This should be `allow runtime pickler lookup`.
                case x => x
              }.asInstanceOf[PickleUnpickleImplementation]
            }
        }
      } else behavior
    } else AlgorithmFailure(s"Cannot use case-class algorithm on non-case class $tpe")
  }
}
