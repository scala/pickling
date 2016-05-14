package scala.pickling.util

object ClassMapper {

  val specializedTuplesTemplate =
    Vector(
      "$mcII$sp", "$mcIJ$sp", "$mcID$sp", "$mcIZ$sp", "$mcJI$sp", "$mcJJ$sp",
      "$mcJD$sp", "$mcJC$sp", "$mcJZ$sp", "$mcDI$sp", "$mcDJ$sp", "$mcDD$sp",
      "$mcDC$sp", "$mcDZ$sp", "$mcCI$sp", "$mcCJ$sp", "$mcCD$sp", "$mcCC$sp",
      "$mcCZ$sp", "$mcZI$sp", "$mcZJ$sp", "$mcZD$sp", "$mcZC$sp", "$mcZZ$sp"
    )

  /* Map specialized classes to classes. Canonical use case: tuples.
   * We map classes instead of strings to check at runtime that they exist. */
  val specialMappingClasses: Map[Class[_], Class[_]] =
    mapSpecializedTuplesFor("scala.Tuple2") // add also other special cases

  def specializedTupleNamesFor(tupleClassName: String): Vector[String] =
    specializedTuplesTemplate.map(tupleClassName + _)

  def mapSpecializedTuplesFor(tupleClassName: String): Map[Class[_], Class[_]] = {
    val tupleClass = Class.forName(tupleClassName)
    specializedTupleNamesFor(tupleClassName)
      .map(Class.forName).map(_ -> tupleClass).toMap
  }

  @inline def isSpecializedClass(specialized: Class[_], clazz: Class[_]) =
    specialMappingClasses.get(specialized).exists(_ == clazz)

  def areSameClasses(clazz: Class[_], clazzT: Class[_]): Boolean =
    clazz == clazzT || isSpecializedClass(clazz, clazzT)

}
