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
private[pickling] sealed trait IrAst {
  /** Returns true if the implementation of this particular AST node requires the use of of
    * a private member, for which we do not have access.
    * @return
    */
  def requiresReflection: Boolean
}

private[pickling] object IrAst {
  def transform(ast: IrAst)(f: IrAst => IrAst): IrAst = {
    def chain(ast: IrAst): IrAst = transform(ast)(f)
    ast match {
      case x: CallConstructor => f(x)
      case x: CallModuleFactory => f(x)
      case x: SetField => f(x)
      case x: GetField => f(x)
      case x: UnpickleSingleton => f(x)
      case x: AllocateInstance => f(x)
      case x: PickleExternalizable => f(x)
      case x: UnpickleExternalizable => f(x)
      case UnpickleBehavior(ops) => f(UnpickleBehavior(ops.map(chain).asInstanceOf[Seq[UnpicklerAst]]))
      case PickleBehavior(ops) => f(PickleBehavior(ops.map(chain).asInstanceOf[Seq[PicklerAst]]))
      case PickleEntry(ops) => f(PickleEntry(ops.map(chain).asInstanceOf[Seq[PicklerAst]]))
      case SubclassUnpicklerDelegation(subs, parent, bOpt, runtime) => f(SubclassUnpicklerDelegation(subs, parent, (bOpt map chain).asInstanceOf[Option[UnpicklerAst]], runtime))
      case SubclassDispatch(subs, parent, bOpt, runtime) => f(SubclassDispatch(subs, parent, (bOpt map chain).asInstanceOf[Option[PicklerAst]], runtime))
      case PickleUnpickleImplementation(p, u) => f(PickleUnpickleImplementation(chain(p).asInstanceOf[PicklerAst], chain(u).asInstanceOf[UnpicklerAst]))
    }
  }
}

/** An AST node representing an operation that can be performed in an unpickler. */
private[pickling] sealed trait UnpicklerAst extends IrAst

/** This represents the pickling library calling the constructor of a class using a set of serialized fields.
  *
  * @param constructor
  *               The method symbol for which constructor to call.
  */
private[pickling] case class CallConstructor(fieldNames: Seq[String], constructor: IrConstructor) extends UnpicklerAst {
  def requiresReflection: Boolean =
    !(constructor.isPublic)
  override def toString = s"constructor (${fieldNames.mkString(", ")})"
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
private[pickling] case class CallModuleFactory(fields: Seq[String], module: IrClass, factoryMethod: IrMethod) extends UnpicklerAst {
  assert(module.isScalaModule)
  assert(!factoryMethod.isStatic)
  def requiresReflection: Boolean =
    factoryMethod.isPublic

  override def toString = s"call $module $factoryMethod (${fields.mkString(", ")})"
}

/** This represents the pickling library SETTING the value of a field, after reading it from the pickle.
  * This can be done either through a "setter" method, or via direct field access.
  *
  * @param setter
  *               The mechanism of setting the value.  Can either directly manipulate a field, or use a method call.
  */
private[pickling] case class SetField(name: String, setter: IrMember) extends UnpicklerAst {
  def requiresReflection: Boolean = !setter.isPublic || ((setter.isScala && setter.isField))
  override def toString = s"set $name w/ $setter"
}

/**
 * When unpickling, this will match against the tag hint to determine what to do.
 * @param subClasses
 *            The set of subclasses that we know about and will auto-lookup an implicit Unpickler to handle.
 * @param parent
 *            The type of what we expect coming.
 * @param parentBehavior
 *            How to unpickle the expected type, if the tag is exactly that (can be None).
 * @param lookupRuntime
 *            True if we should generate code to hit the runtime unpicklers, otherwise unknown tags will lead to an error.
 */
private[pickling] case class SubclassUnpicklerDelegation(subClasses: Seq[IrClass], parent: IrClass, parentBehavior: Option[UnpicklerAst] = None, lookupRuntime: Boolean = false) extends UnpicklerAst {
  def requiresReflection: Boolean = false
  override def toString = {
    val cases = (
        parentBehavior.toList.map(b => s"case thisClass =>\n  $b") ++
        subClasses.map(c => s" case $c => lookup implicit unpickler $c") ++
        (if(lookupRuntime) List("case _ => lookup runtime") else List("case _ => error"))
      )
    s"clazz match {${cases.mkString("\n", "\n", "\n")}}"
  }
}

/** A set of behaviors used to implement unpickling. */
private[pickling] case class UnpickleBehavior(operations: Seq[UnpicklerAst]) extends UnpicklerAst {
  def requiresReflection = operations exists (_.requiresReflection)
  override def toString = s"unpickle behavior {${operations.mkString("\n", "\n", "\n")}}"
}

/** A raw `Unsafe.allocateInstance` call for a given type/class. */
private[pickling] case class AllocateInstance(tpe: IrClass) extends UnpicklerAst {
  def requiresReflection = true
}

/** Unpickle a singleton type. */
private[pickling] case class UnpickleSingleton(tpe: IrClass) extends UnpicklerAst {
  def requiresReflection: Boolean = {
    // TODO - check to see if the tpe is private....
    false
  }
  override def toString = s"get singleton $tpe"
}

/** This is an AST node representing an operation performed by a pickler. */
private[pickling] sealed trait PicklerAst extends IrAst

/**
 * This represents the pickling library GETTING the value of a field, and writing out to the pickle.
 * This can be done either through a getting metod, or via direct field access.
 * @param getter
 */
private[pickling] case class GetField(name: String, getter: IrMember) extends PicklerAst {
  // NOTE; We do not know how to handle non-reflective field access for scala symbols yet.
  def requiresReflection: Boolean = !getter.isPublic || (getter.isScala && getter.isField)
  override def toString = s"get field $name from $getter"
}

/** This denotes that for pickling we should delgate the behavior beased on the runtime class.
  *
  * @param subClasses  The subclasses we KNOW about and can delegate directly to picklers
  *                    We will require that these have implicitly available picklers.
  * @param parent
  *                    The parent class we're dispatching against.
  * @param parentBehavior
  *                     If passed,This represents the behavior of pickling for the current class.
  *                     i.e. if parent.tpe is matched at runtime, this is the beavhior used.
  *                     If None, we shouldn't even try to pickle the current class.
  * @param lookupRuntime
  *                      True if we are allowed to generate/lookup picklers are runtime.
  */
private[pickling] case class SubclassDispatch(subClasses: Seq[IrClass], parent: IrClass, parentBehavior: Option[PicklerAst] = None, lookupRuntime: Boolean = false) extends PicklerAst {
  def requiresReflection: Boolean = false
  override def toString = {
    val cases: Seq[String] =
      (subClasses.map(c => s"case $c => implicitly pickle") ++
        parentBehavior.toList.map(b => s"case thisClass =>\n$b") ++
        (if(lookupRuntime) List("case _ => lookup runtime")
         else List("case _ => error")))
    s"class match {${cases.mkString("\n", "\n", "\n")}"
  }
}
/** Hardcoded pickling of Externalizable classes using built in magik. */
private[pickling] case class PickleExternalizable(tpe: IrClass) extends PicklerAst {
  def requiresReflection: Boolean = false
}
/** Hardcoded unpickling of Externalizable classes using built in magik classes. */
private[pickling] case class UnpickleExternalizable(tpe: IrClass) extends UnpicklerAst {
  // We use unsafe to instantiate
  def requiresReflection: Boolean = true
}


/** Ensure that beginEntry/hintOid (sharing/ref)/endEntry are called around the nested operations. */
private[pickling] case class PickleEntry(ops: Seq[PicklerAst]) extends PicklerAst {
  def requiresReflection: Boolean = ops exists (_.requiresReflection)
  override def toString = s"  entry {${ops.mkString("\n", "\n", "\n")}}"
}

/** This represents the algorithm used to pickle a given class. */
private[pickling] case class PickleBehavior(operations: Seq[PicklerAst]) extends PicklerAst {
  def requiresReflection: Boolean = operations.exists(_.requiresReflection)
  override def toString = s"pickle behavior {${operations.mkString("\n", "\n", "\n")}}"
}

private[pickling] case class PickleUnpickleImplementation(pickle: PicklerAst, unpickle: UnpicklerAst) extends IrAst {
  override def requiresReflection: Boolean = pickle.requiresReflection || unpickle.requiresReflection
  override def toString = s"$pickle\n---$unpickle\n"
}