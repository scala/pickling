package scala.pickling
package internal

import java.lang.reflect.{Method, Field}

/**
 * Helper method to aid in retrieving [[java.lang.reflect.Field]]s and [[java.lang.reflect.Method]]s at runtime.
 *
 */
object Reflect {

  // TODO - Ideally we should pass an owner here too.
  /**
   * Finds a field (it could be private) in this class and returns an instance.
   * @param cls   The class to inspect.
   * @param name  The name of the field.
   * @return  The field
   *
   * @throws scala.pickling.PicklingException this is thrown if the field is not found.
   */
  def getField(top: Class[_], name: String): Field = {
    def getFieldHelper(cls: Class[_]): Field = {
      try cls.getDeclaredField(name)
      catch {
        case nsf: NoSuchFieldException =>
          if ((cls.getSuperclass != null) && (cls.getSuperclass != classOf[Object]) && cls.getSuperclass != cls) getFieldHelper(cls.getSuperclass)
          else throw new PicklingException(s"Could not find field [$name] in [$cls]")
      }
    }
    getFieldHelper(top)
  }

  def getMethod(top: Class[_], name: String, args: Array[Class[_]]): Method= {
    def getMethodHelper(cls: Class[_]): Method = {
      try cls.getDeclaredMethod(name, args: _*)
      catch {
        case nsf: NoSuchMethodException =>
          if ((cls.getSuperclass != null) && cls.getSuperclass != classOf[Object] && cls.getSuperclass != cls) getMethodHelper(cls.getSuperclass)
          else throw new PicklingException(s"Could not find method [$name(${args.mkString(", ")})] in [$cls]")
      }
    }
    getMethodHelper(top)
  }
}
