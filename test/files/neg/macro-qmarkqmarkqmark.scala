import language.experimental.macros

object Macros {
  def foo1 = macro ???
  foo1

  def foo2(x: Int) = macro ???
  foo2
  foo2(1)

  def foo3[T] = macro ???
  foo3[Int]

  type Foo1 = macro ???
  class C1 extends Foo1

  type Foo2(x: Int) = macro ???
  class C21 extends Foo2
  class C22 extends Foo2(2)

  type Foo3[T] = macro ???
  class C3 extends Foo3[String]
}