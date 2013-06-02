package scala.testing

trait PicklingBenchmark extends Benchmark {
  val size = System.getProperty("size").toInt
}