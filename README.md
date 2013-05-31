scala-pickling paradise
=======================

This project aims to turn [a custom build of macro paradise](https://github.com/heathermiller/scala-pickling) that we used in
[Object-Oriented Pickler Combinators and an Extensible Generation Framework](http://lampwww.epfl.ch/~hmiller/files/pickling.pdf)
into a standalone library that targets [Macro Paradise 2.11](http://docs.scala-lang.org/overviews/macros/paradise.html#macro_paradise_for_211x).

Known limitations:
  1. ~~No support for `@pickleable`.~~
  2. In the public API (and everywhere else), vanilla type tags are replaced with `scala.pickling.FastTypeTag/scala.pickling.fastTypeTag`.
  3. Picklers are generated directly at call sites.
  4. ~~No runtime compilation, since it's not obvious how to package it without inducing a dependency on `scala-compiler.jar`.~~
  5. No classpath scans, because they don't work.