package scala.pickling
package tags

private[pickling] final case class SimpleFastTypeTag[T](
    typeConstructor: String, 
    typeArgs: List[FastTypeTag[_]]) extends FastTypeTag[T] {
  override def isSimpleType = typeArgs.isEmpty
  override val key = 
    if (typeArgs.isEmpty) typeConstructor 
    else s"$typeConstructor[${typeArgs.map(_.key).mkString(",")}]"
}
