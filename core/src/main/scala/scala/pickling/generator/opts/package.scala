package scala.pickling.generator

trait IsIgnoreCaseClassSubclasses

/**
 * This gives us all of our generator options.
 */
package object opts {
  implicit object ignoreCaseClassSubclasses extends IsIgnoreCaseClassSubclasses

}
