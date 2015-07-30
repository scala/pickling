package scala.pickling
package internal

import scala.reflect.ClassTag

private[pickling] object Classes {

  private[pickling] def classTagFromString(typeString: String): ClassTag[_] = {
    if (typeString.startsWith("scala.Array")) {
      val elemTypeString = typeString.substring(12, typeString.length - 1)
      val elemClassTag   = classTagFromString(elemTypeString)
      elemClassTag.wrap
    } else {
      val clazz = typeString match {
        case "scala.Double" => classOf[Double]
        case _ => Class.forName(typeString)
      }
      ClassTag(clazz)
    }
  }

  private[pickling] def classFromString(typeString: String): Class[_] = {
    classTagFromString(typeString).runtimeClass
  }

}
