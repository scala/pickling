package scala.tools.reflect

import scala.reflect.macros.contexts.Context

abstract class Quasiquotes extends ApplyMacro
                              with UnapplyMacro
                              with Parsers
                              with Reifiers  {
  val c: Context
  val global: c.universe.type = c.universe
}
