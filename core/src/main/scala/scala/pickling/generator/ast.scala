package scala.pickling
package generator



// 1. Read symbols for type
// 2. Use an algorithm to determine the behavior of the pickle/unpickle methods (i..e generate an AST)
//     CallConstructor(V)
//     SetField(<date>)
//     SetField(<id>)
//     ---------------
//     GetField(<date>)
//     GetField(<id>)
// 3. Generate code using IrAst


/**
 * An AST representing simple pickling behavior.  We generate this before we generate the implementing code.
 *
 *
 */
sealed trait IrAst {
  /** Returns true if the implementation of this particular AST node requires the use of of
    * a private member, for which we do not have access.
    * @return
    */
  def requiresReflection: Boolean
}

/** An AST node representing an operation that can be performed in an unpickler. */
sealed trait UnpicklerAst extends IrAst

/** This represents the pickling library calling the constructor of a class using a set of serialized fields.
  *
  * @param constructor
  *               The method symbol for which constructor to call.
  */
case class CallConstructor(fieldNames: Seq[String], constructor: IrConstructor) extends UnpicklerAst {
  def requiresReflection: Boolean =
    !(constructor.isPublic)
}

/** This represents grabing a scala module and calling a factory method on it.
  *
  * @param fields
  *         The fields that should be deserialized IN THE ORDER SPECIFIED.
  *               i.e. an unpickler should deserialize these fields in the same order specified here, then
  *               pass them in that same order to the constructor.
  * @param factoryMethod
  *          The method to call which will construct an instance of the class.  This must be defined on a Scala module.
  */
case class CallModuleFactory(fields: Seq[String], module: IrClass, factoryMethod: IrMethod) extends UnpicklerAst {
  assert(module.isScalaModule)
  assert(!factoryMethod.isStatic)
  def requiresReflection: Boolean =
    factoryMethod.isPublic
}

/** This represents the pickling library SETTING the value of a field, after reading it from the pickle.
  * This can be done either through a "setter" method, or via direct field access.
  *
  * @param setter
  *               The mechanism of setting the value.  Can either directly manipulate a field, or use a method call.
  */
case class SetField(name: String, setter: IrMember) extends UnpicklerAst {
  def requiresReflection: Boolean = !setter.isPublic
}
/** A set of behaviors used to implement unpickling. */
case class UnpickleBehavior(operations: Seq[UnpicklerAst]) extends UnpicklerAst {
  def requiresReflection = operations exists (_.requiresReflection)
}

/** This is an AST node representing an operation performed by a pickler. */
sealed trait PicklerAst extends IrAst

/**
 * This represents the pickling library GETTING the value of a field, and writing out to the pickle.
 * This can be done either through a getting metod, or via direct field access.
 * @param getter
 */
case class GetField(name: String, getter: IrMember) extends PicklerAst {
  def requiresReflection: Boolean = !getter.isPublic
}

// TODO - What to do with unknown dispatch?
case class SubclassDispatch(subClasses: Seq[IrClass], parent: IrClass) extends PicklerAst {
  def requiresReflection: Boolean = false
}
/** Ensure that beginEntry/hintOid (sharing/ref)/endEntry are called around the nested operations. */
case class PickleEntry(ops: Seq[PicklerAst]) extends PicklerAst {
  def requiresReflection: Boolean = ops exists (_.requiresReflection)
}

/** This represents the algorithm used to pickle a given class. */
case class PickleBehavior(operations: Seq[PicklerAst]) extends PicklerAst {
  def requiresReflection: Boolean = operations.exists(_.requiresReflection)
}

case class PickleUnpickleImplementation(pickle: PicklerAst, unpickle: UnpicklerAst)