package scala.pickling
package generator


// This should be able to serialize singleton scala objects.
private[pickling] object ScalaSingleton extends PicklingAlgorithm {
  override def generate(tpe: IrClass, logger: AlgorithmLogger): AlgorithmResult = {
    if(tpe.isScalaModule) {
      AlgorithmSucccess(PickleUnpickleImplementation(
        PickleBehavior(Seq(PickleEntry(Seq()))),
        UnpickleBehavior(Seq(UnpickleSingleton(tpe)))
      ))
    } else AlgorithmFailure(s"$tpe is not a singleton scala object")
  }
}