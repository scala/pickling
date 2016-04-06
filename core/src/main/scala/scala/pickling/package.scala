package scala

package object pickling {
  type FastTypeTag[T] = tags.FastTypeTag[T]
  val FastTypeTag = tags.FastTypeTag
}
