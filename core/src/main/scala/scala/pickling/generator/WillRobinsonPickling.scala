package scala.pickling
package generator

/** This algorithm attempts to be able to pickle ANY class just be grabbing all its fields.
  *
  *
  * Notes:
  * - https://en.wikipedia.org/wiki/Danger,_Will_Robinson
  * - https://www.youtube.com/watch?v=RG0ochx16Dg
  */
private[pickling] class WillRobinsonPickling(showWarnings: Boolean) extends PicklingAlgorithm {
  private case class FieldInfo(setter: SetField, getter: GetField)
  // TODO - Constructor unification in the case-class generator is probably still useful here...
  private def allScalaField(tpe: IrClass, logger: AlgorithmLogger): Seq[FieldInfo] = {
    val fields =
      // Note: For some reason, we sometimes see the same field twice, so we filter these out here.
      //       This is probably a bug somewhere.
      IrSymbol.allDeclaredFieldsIncludingSubclasses(tpe).filterNot(_.isMarkedTransient).toList.
        // TODO - This uniqueness enforcement is probably due to errors in the symbol loading, maybe should be enforced there.
        groupBy(_.fieldName).map(_._2.head).toList.
        sortBy(_.fieldName)
    fields.map{ f =>
      FieldInfo(SetField(f.fieldName, f), GetField(f.fieldName, f))
    }
  }

  override def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    if(showWarnings) logger.warn(s"DANGER WILL ROBINSON - ${tpe} is being serialized/deserialized using Unsafe operations.  Cannot statically identify any other safe mechanism.")
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
      // We special case AnyRef to be PURE reflection-based pickling.
    } else if((tpe.className == "java.lang.Object") || (tpe.className == "AnyRef")) {
      val pickle =
        SubclassDispatch.apply(Nil, tpe, None, lookupRuntime = true)
      val unpickle =
        SubclassUnpicklerDelegation.apply(Nil, tpe, None, lookupRuntime = true)
      AlgorithmSucccess(PickleUnpickleImplementation(pickle, unpickle))
    } else

    // TODO - Grab a list of all vars and vals and serialized them ALL.
    //        We're planning to unsafe instantiate (no constructor) anyway.
    AlgorithmFailure(s"Pickling arbitrary java type ($tpe) not implemented yet.")
  }
}
