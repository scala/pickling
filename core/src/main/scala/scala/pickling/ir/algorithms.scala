package scala.pickling
package ir

import scala.reflect.api.Universe

/** An abstract implementation of a pickling generation algorithm.
  *
  *
  * TODO - Do we even need an interfaace?
  */
trait PicklingAlgorithm {
  /**
   * Attempts to construct pickling logic for a given type.
   */
  def generatePickler(tpe: IrClass): Option[PicklerAst]
  def generateUnpickler(tpe: IrClass): Option[UnpicklerAst]
}

/** this algorithm inspects symbols to determine if we have a scala case class, and generates the
  * pickling code for it.
  *
  * This ONLY handles case-class types, it will not handle ADTS.
  */
object CaseClassPickling extends PicklingAlgorithm {
  case class FieldInfo(name: String, sym: IrMethod)
  case class CaseClassInfo(constructor: IrConstructor, fields: Seq[FieldInfo])
  private def getFieldsAndContructor(tpe: IrClass): Option[CaseClassInfo] = {
    if(tpe.isCaseClass) {
      if(!tpe.isFinal) {
        // TODO - Issue real warning about how we don't handle subclassing behavior when working with case classes.
        System.err.println(s"Warning: ${tpe.className} is not final.  Generated unpickling code does not handle subclasses.")
      }
      tpe.primaryConstructor map { c =>
        val names = c.parameterNames.toSet
        val hasStandaloneVar = tpe.methods.exists { m =>
          m.isVar && !(names contains m.methodName)
        }
        if(hasStandaloneVar) {
          // TODO - Issue a real warning about standalone vars, and how we don't handle them.
          System.err.println(s"Warning: ${tpe.className} has a member var.  Pickling is not guaranteed to handle this correctly.")
        }

        // Here we need to unify the fields with the constructor names.  We assume they have the same name.
        val fields = for {
          name <- c.parameterNames.toSeq
          m <- tpe.methods.find(_.methodName == name)
        } yield FieldInfo(name, m)
        if(fields.length == c.parameterNames.length) CaseClassInfo(c, fields)
        else ??? // TODO - what do we do if it doesn't line up?  This is probably some insidious bug.
      }

    } else None
  }


  /**
   * Attempts to construct pickling logic fora  given type.
   * @param tpe
   * @return
   */
  override def generatePickler(tpe: IrClass): Option[PicklerAst] = {
    getFieldsAndContructor(tpe) map { structure =>
      PickleBehavior(structure.fields.map { field =>
        GetField(field.name, field.sym)
      }.toSeq)
    }
  }
  override def generateUnpickler(tpe: IrClass): Option[UnpicklerAst] =
    getFieldsAndContructor(tpe) map { structure => CallConstructor(structure.fields.map(_.name), structure.constructor) }

}