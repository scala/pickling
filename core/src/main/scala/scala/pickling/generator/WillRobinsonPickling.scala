package scala.pickling
package generator

/** This algorithm attempts to be able to pickle ANY class just be grabbing all its fields.
  *
  *
  * Notes:
  * - https://en.wikipedia.org/wiki/Danger,_Will_Robinson
  * - https://www.youtube.com/watch?v=RG0ochx16Dg
  */
object WillRobinsonPickling extends PicklingAlgorithm {
  private case class FieldInfo(setter: SetField, getter: GetField)
  private def allScalaField(tpe: IrClass, logger: AlgorithmLogger): Seq[FieldInfo] = {
    // TODO - We find all these and hope it's ok
    val fields = tpe.methods.filter(m => m.isVal || m.isVar).toList.sortBy(_.methodName)
    fields.flatMap { f =>
      f.setter match {
        case None =>
          logger.warn(s"Field $f has no setter.  $tpe may not be serialized correctly")
          Nil
        case Some(s) =>
          Seq(FieldInfo(SetField(f.methodName, s), GetField(f.methodName, f)))
      }
    }
  }

  override def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    logger.warn(s"DANGER WILL ROBINSON - ${tpe} is being serialized/deserialized using Unsafe operations.  Cannot statically identify any other safe mechanism.")
    if(tpe.isScala) {
      // TODO - We should probably try the constructor unification thing.
      val fields = allScalaField(tpe, logger)
      val unpickleBasic =
        UnpickleBehavior(
          Seq(AllocateInstance(tpe)) ++
          fields.map(f => f.setter).toSeq)
      val pickleBasic =
        PickleEntry((fields.map(f => f.getter)))


      val pickle =
        SubclassDispatch.apply(Nil, tpe, Some(pickleBasic), lookupRuntime = true)
      val unpickle =
        SubclassUnpicklerDelegation.apply(Nil, tpe, Some(unpickleBasic), lookupRuntime = true)
      AlgorithmSucccess(PickleUnpickleImplementation(pickle, unpickle))
    } else

    // TODO - Grab a list of all vars and vals and serialized them ALL.
    //        We're planning to unsafe instantiate (no constructor) anyway.
    AlgorithmFailure(s"Pickling arbitrary types not implemented yet.")
  }
}