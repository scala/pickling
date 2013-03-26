package scala.reflect.synthetic

object ReifiedScalaOption {
  val tag = scala.reflect.runtime.universe.typeTag[Option[Any]]
}