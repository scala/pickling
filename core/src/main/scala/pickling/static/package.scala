package scala.pickling

sealed trait IsStaticOnly

package object static {
  implicit object StaticOnly extends IsStaticOnly
}
