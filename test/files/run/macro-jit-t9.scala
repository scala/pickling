import scala.reflect.macros.Context
import language.experimental.macros

object Test extends App {
  def impl(c: Context) = c.universe.reify(println("hello world!"))
  def hello = macro impl
  hello
}
