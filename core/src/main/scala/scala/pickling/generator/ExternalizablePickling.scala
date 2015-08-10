package scala.pickling.generator

/**
 * This implements an algorithm which will handle Externalizable classes.  It's not
 * the most efficient, but it does work and has tests.
 */
private[pickling] class ExternalizablePickling extends PicklingAlgorithm {

  def isExternalizable(tpe: IrClass): Boolean = {
    (tpe.className == "java.io.Externalizable") ||
    tpe.parentClasses.exists(isExternalizable)
  }
  /**
   * Attempts to construct pickling logic for a given type.
   */
  override def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    if(isExternalizable(tpe)) {
      logger.warn(s"Using Externalizable interface for $tpe.  This may be less efficient than writing your own pickler/unpickler.")
      AlgorithmSucccess(PickleUnpickleImplementation(
        pickle = PickleEntry(Seq(PickleExternalizable(tpe))),
        unpickle = UnpickleExternalizable(tpe)
      ))
    } else AlgorithmFailure(s"$tpe does not extend java.io.Externalizable")
  }
}
