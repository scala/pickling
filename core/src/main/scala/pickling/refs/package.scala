package scala.pickling

package object norefs {
  implicit val NoRefs: refs.NoRefs = new refs.NoRefs{}
}

package refs {
  trait Ref

  trait NoRefs
}