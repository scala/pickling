scala-pickling paradise
=======================

**Scala Pickling** is an automatic serialization framework made for Scala. It's fast, boilerplate-free, and allows users to easily swap in/out different serialization formats (such as binary, or JSON), or even to provide your own custom serialization format.

Basic usage:

    import scala.pickling._
    import json._

    val pckl = List(1, 2, 3, 4).pickle
    val lst = pckl.unpickle[List[Int]]

## Quick Start

- make sure `scala-pickling.jar` is on your classpath
- use Scala 2.10.2

## What makes it different?

Scala Pickling...

- can be **Language-Neutral** if you want it to be. Changing the format of your serialized data is as easy as importing the correct implicit pickle format into scope. Out of the box, we currently support a fast Scala binary format, as well as JSON. Support is currently planned for other formats. Or, you can even roll your own custom pickle format!
- is **Automatic**. That is, without any boilerplate at all, one can instruct the framework to figure out how to serialize an arbitrary class instance. No need to register classes, no need to implement any methods.
- **Allows For Unanticipated Evolution**. That means that you don’t have to extend some marker trait in order to serialize a given Scala class. Just import the scala.pickling package and call pickle on the instance that you would like to serialize.
- gives you more **Typesafety**. No more errors from serialization/deserialization propagating to arbitrary points in your program. Unlike Java Serialization, errors either manifest themselves as compile-time errors, or runtime errors only at the point of unpickling.
- has **Robust Support For Object-Orientation**. While Scala Pickling is based on the elegant notion of pickler combinators from functional programming, it goes on to extend pickler combinators to be able to handle subtyping, a notion which does not exist in the purely functional paradigm. That means that if you pickle an instance of a subclass, and then try to unpickle as an instance of a superclass, you will still get back the original subclass which you initially pickled.
- **Happens At Compile-Time**. That means that it’s super-performant because serialization-related code is typically generated at compile-time and inlined where it is needed in your code. Scala Pickling is essentially fully-static, reflection is only used as a fallback when static (compile-time) generation fails.

<!-- This project aims to turn [a custom build of macro paradise](https://github.com/heathermiller/scala-pickling/tree/topic/scala-pickling) that we used in
[Object-Oriented Pickler Combinators and an Extensible Generation Framework](http://lampwww.epfl.ch/~hmiller/files/pickling.pdf)
into a standalone library that targets [Macro Paradise 2.11](http://docs.scala-lang.org/overviews/macros/paradise.html#macro_paradise_for_211x).

Known limitations:
  1. ~~No support for `@pickleable`.~~
  2. In the public API (and everywhere else), vanilla type tags are replaced with `scala.pickling.FastTypeTag/scala.pickling.fastTypeTag`.
  3. Picklers are generated directly at call sites, since we cannot have introduceTopLevel in 2.10.x.
  4. No runtime compilation, since it's not obvious how to package it without inducing a dependency on `scala-compiler.jar`.
 -->
