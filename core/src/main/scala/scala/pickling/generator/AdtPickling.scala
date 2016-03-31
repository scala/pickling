package scala.pickling
package generator


/** This algorithm isnpects symbols to determine if we have an abstract class with fully known sub-classes,
  * in which case we delegate all behavior to the subclasses.
  */
private[pickling] object AdtPickling extends PicklingAlgorithm {
  /**
   * Attempts to construct pickling logic for a given type.
   */
  override def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    if(!tpe.isAbstract) {
      AlgorithmFailure(s"Cannot use ADT algorithm because $tpe is not abstract")
    } else tpe.closedSubclasses match {
      case scala.util.Failure(msgs) =>
        AlgorithmFailure(s"Could not determine if $tpe is closed for ADT generation:\n\t\t$msgs")
      case scala.util.Success(Seq()) =>
        AlgorithmFailure(s"Failed to create ADT pickler for $tpe.  Type is closed, but could not find subclasses.\n  You can use @directSubclasses to annotate known subclasses.")
      case scala.util.Success(subclasses) =>
        val pickle = PickleBehavior(Seq(SubclassDispatch(subclasses, tpe)))
        val unpickle = UnpickleBehavior(Seq(SubclassUnpicklerDelegation(subclasses, tpe)))
        AlgorithmSucccess(PickleUnpickleImplementation(pickle, unpickle))
    }
  }
}