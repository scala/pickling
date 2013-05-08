scala-pickling 2.10.x
=====================

This project aims to turn [a custom build of macro paradise](https://github.com/heathermiller/scala-pickling) that we used in
[Object-Oriented Pickler Combinators and an Extensible Generation Framework](http://lampwww.epfl.ch/~hmiller/files/pickling.pdf)
into a standalone library that targets 2.10.x
(more precisely, the library will require 2.10.2+, since it's when implicit macros, a key ingredient to our technique,
[have been fixed](https://issues.scala-lang.org/browse/SI-5923)).

Since we are making extensive use of quasiquotes, which cannot be added to 2.10.x due to binary and source compatibility
restrictions, building (but not using!) scala-pickling-210x requires [a custom build of scalac](https://github.com/scalamacros/kepler/tree/topic/pickling),
as explained in comments to [Build.scala](https://github.com/xeno-by/scala-pickling-210x/blob/master/project/Build.scala).
Please note that scala-pickling-210x can be used without any problems with vanilla scalac 2.10.2 -
custom scalac is only necessary to _build_ this library, not to compile against it or to link to it at runtime.

Known limitations:
  1. No support for `@pickleable`, since we cannot have macro annotations in 2.10.x.
  2. In the public API (and everywhere else), vanilla type tags are replaced with `scala.pickling.FastTypeTag/scala.pickling.fastTypeTag`.
  3. Picklers are generated directly at call sites, since we cannot have introduceTopLevel in 2.10.x.
  4. No runtime compilation, since it's not obvious how to package it without inducing a dependency on `scala-compiler.jar`.
  5. No classpath scans, because they don't work.