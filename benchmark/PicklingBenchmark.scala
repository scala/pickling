package scala.pickling.testing

trait PicklingBenchmark extends Benchmark {

  override def setUp(): Unit = {
    super.setUp()
    // Here we just access the currentRuntime to ensure that it is loaded before measuring.
    val runtime = scala.pickling.internal.currentRuntime
    runtime.refRegistry.pickle.clear()
    // Here we force the cake to bake.  This can drop 1100ms off the startup time due to super inefficient cakery baking
    val x = scala.pickling.Defaults.refUnpickler
  }

  val size = System.getProperty("size").toInt
}
object PicklingBenchmark