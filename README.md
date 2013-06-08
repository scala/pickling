scala-pickling
==============

 **Fast, Customizable, Boilerplate-Free Serialization for Scala**

This project aims to turn [a custom build of macro paradise](https://github.com/heathermiller/scala-pickling/tree/topic/scala-pickling) that we used in
[Object-Oriented Pickler Combinators and an Extensible Generation Framework](http://lampwww.epfl.ch/~hmiller/files/pickling.pdf)
into a standalone library that targets 2.10.x
(The library requires 2.10.2+).

Since we are making extensive use of quasiquotes, which cannot be added to 2.10.x due to binary and source compatibility
restrictions, building (but not using!) scala-pickling requires [a custom build of scalac](http://docs.scala-lang.org/overviews/macros/paradise.html#macro_paradise_for_210x).
Please note that scala-pickling can be used without any problems with vanilla scalac 2.10.2 -
custom scalac is only necessary to _build_ this library, not to compile against it or to link to it at runtime.

Known limitations:
  1. No support for `@pickleable`, since we cannot have macro annotations in 2.10.x.
  2. In the public API (and everywhere else), vanilla type tags are replaced with `scala.pickling.FastTypeTag/scala.pickling.fastTypeTag`.
  3. Picklers are generated directly at call sites, since we cannot have introduceTopLevel in 2.10.x.
  4. No runtime compilation, since it's not obvious how to package it without inducing a dependency on `scala-compiler.jar`.
