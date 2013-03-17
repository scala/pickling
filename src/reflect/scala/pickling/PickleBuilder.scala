package scala.pickling

import scala.reflect.runtime.universe._

trait PickleBuilder {
  type PickleType <: Pickle
  def beginPickle(tpe: Type, picklee: Any): this.type
  def putField(name: String, value: this.type => this.type): this.type
  def endPickle(): Unit
  def result(): PickleType // same name as collection.mutable.Builder
}
