scala-pickling 2.10.x
=====================

This project aims to turn [a custom build of macro paradise](https://github.com/heathermiller/scala-pickling) that we used in 
[Object-Oriented Pickler Combinators and an Extensible Generation Framework](http://lampwww.epfl.ch/~hmiller/files/pickling.pdf)
into a standalone library that targets 2.10.x 
(more precisely, the library will require 2.10.2+, since it's when implicit macros, a key ingredient to our technique,
[have been fixed](https://issues.scala-lang.org/browse/SI-5923)).

