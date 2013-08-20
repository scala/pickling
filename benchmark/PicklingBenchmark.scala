package scala.pickling.testing

trait PicklingBenchmark extends Benchmark {
  val size = System.getProperty("size").toInt
}