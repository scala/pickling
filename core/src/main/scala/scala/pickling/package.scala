package scala

package object pickling {

  type FastTypeTag[T] = tags.FastTypeTag[T]
  val FastTypeTag = tags.FastTypeTag

  /* Import here so that we don't break existing code since
   * the location of the errors is now defined independently. */
  type PicklingException = PicklingErrors.BasePicklingException
  val PicklingException = PicklingErrors.BasePicklingException

}
