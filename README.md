scala/pickling
==============

[![Build Status](https://travis-ci.org/scala/pickling.svg?branch=0.10.x)](https://travis-ci.org/scala/pickling/)
[![Stories in Ready](https://badge.waffle.io/scala/pickling.png?label=ready&title=Ready)](http://waffle.io/scala/pickling)
[![Join the chat at https://gitter.im/scala/pickling](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scala/pickling?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**Scala Pickling** is an automatic serialization framework made for Scala. It's fast, boilerplate-free, and allows users to easily swap in/out different serialization formats (such as binary, or JSON), or even to provide their own custom serialization format.

### Defaults mode

```scala
scala> import scala.pickling.Defaults._, scala.pickling.json._
scala> case class Person(name: String, age: Int)

scala> val pkl = Person("foo", 20).pickle
pkl: pickling.json.pickleFormat.PickleType =
JSONPickle({
  "$type": "Person",
  "name": "foo",
  "age": 20
})

scala> val person = pkl.unpickle[Person]
person: Person = Person(foo,20)
```

For more, flip through, or watch the [ScalaDays 2013 presentation!](http://www.parleys.com/play/51c3799fe4b0d38b54f4625a/chapter0/about)
<br> For deeper technical details, we've also written an OOPSLA 2013 paper on scala/pickling, [Instant Pickles: Generating Object-Oriented Pickler Combinators for Fast and Extensible Serialization](http://infoscience.epfl.ch/record/187787/files/oopsla-pickling_1.pdf).

## Get Scala Pickling

Scala Pickling is available on Sonatype for Scala 2.10 and Scala 2.11!
You can use Scala Pickling in your sbt project by simply adding the following dependency to your build file:

```scala
libraryDependencies += "org.scala-lang.modules" %% "scala-pickling" % "0.10.1"
```

## What makes it different?

Scala Pickling...

- can be **Language-Neutral** if you want it to be. Changing the format of your serialized data is as easy as importing the correct implicit pickle format into scope. Out of the box, we currently support a fast Scala binary format, as well as JSON. Support is currently planned for other formats. Or, you can even roll your own custom pickle format!
- is **Automatic**. That is, without any boilerplate at all, one can instruct the framework to figure out how to serialize an arbitrary class instance. No need to register classes, no need to implement any methods.
- **Allows For Unanticipated Evolution**. That means that you don’t have to extend some marker trait in order to serialize a given Scala class. Just import the scala.pickling package and call pickle on the instance that you would like to serialize.
- gives you more **Typesafety**. No more errors from serialization/deserialization propagating to arbitrary points in your program. Unlike Java Serialization, errors either manifest themselves as compile-time errors, or runtime errors only at the point of unpickling.
- has **Robust Support For Object-Orientation**. While Scala Pickling is based on the elegant notion of pickler combinators from functional programming, it goes on to extend the traditional form of pickler combinators to be able to handle open class hierarchies. That means that if you pickle an instance of a subclass, and then try to unpickle as a superclass, you will still get back an instance of the original subclass.
- **Happens At Compile-Time**. That means that it’s super-performant because serialization-related code is typically generated at compile-time and inlined where it is needed in your code. Scala Pickling is essentially fully-static, reflection is only used as a fallback when static (compile-time) generation fails.

## Optimizing performance

Pickling enables optimizing performance through configuration, in case the pickled objects are known to be simpler than in the general case.

### Disabling cyclic object graphs

By default, Pickling can serialize cyclic object graphs (for example, for serializing doubly-linked lists). However, this requires bookkeeping at run time. If pickled objects are known to be *not cyclic* (for example, simple lists or trees), then this additional bookkeeping can be disabled using the following import:

```scala
import scala.pickling.shareNothing._
```

If objects are pickled in a tight loop, this import can lead to a significant performance improvement.

### Static serialization without reflection

To pickle objects of types like `Any` Pickling uses run-time reflection, since not enough information is available at compile time. However, Pickling supports a static-only mode that ensures *no run-time reflection* is used. In this mode, pickling objects that would otherwise require run-time reflection causes compile-time errors.

The following import enables static-only serialization:

```scala
import scala.pickling.static._  // Avoid run-time reflection
```

## A la carte import

If you want, Pickling lets you import specific parts (functions, ops, picklers, and format) so you can customize each part.

```scala
import scala.pickling._         // This imports names only
import scala.pickling.json._    // Imports PickleFormat
import scala.pickling.static._  // Avoid runtime pickler

// Import pickle ops
import scala.pickling.Defaults.{ pickleOps, unpickleOps } 
// Alternatively import pickle function
// import scala.pickling.functions._

// Import picklers for specific types
import scala.pickling.Defaults.{ stringPickler, intPickler, refUnpickler, nullPickler }

case class Pumpkin(kind: String)
// Manually generate a pickler using macro
implicit val pumpkinPickler = Pickler.generate[Pumpkin]
implicit val pumpkinUnpickler = Unpickler.generate[Pumpkin]

val pckl = Pumpkin("Kabocha").pickle
val pump = pckl.unpickle[Pumpkin]
```

## DIY protocol stack

There are also traits available for picklers to mix and match your own convenience object to import from.
If you're a library author, you can provide the convenience object as your protocol stack that some or all of the pickling parts:

- ops
- functions
- picklers
- format

```scala
scala> case class Apple(kind: String)
defined class Apple

scala> val appleProtocol = {
     |              import scala.pickling._
     |              new pickler.PrimitivePicklers with pickler.RefPicklers
     |                  with json.JsonFormats {
     |                // Manually generate pickler for Apple
     |                implicit val applePickler = PicklerUnpickler.generate[Apple]
     |                // Don't fall back to runtime picklers
     |                implicit val so = static.StaticOnly
     |                // Provide custom functions
     |                def toJsonString[A: Pickler](a: A): String =
     |                  functions.pickle(a).value
     |                def fromJsonString[A: Unpickler](s: String): A =
     |                  functions.unpickle[A](json.JSONPickle(s))
     |              }
     |            }
appleProtocol: scala.pickling.pickler.PrimitivePicklers with scala.pickling.pickler.RefPicklers with scala.pickling.json.JsonFormats{implicit val applePickler: scala.pickling.Pickler[Apple] with scala.pickling.Unpickler[Apple] with scala.pickling.Generated; implicit val so: scala.pickling.static.StaticOnly.type; def toJsonString[A](a: A)(implicit evidence$1: scala.pickling.Pickler[A]): String; def fromJsonString[A](s: String)(implicit evidence$2: scala.pickling.Unpickler[A]): A} = $anon$1@2b033c35
```

Now your library user can import `appleProtocol` as follows:

```
scala> import appleProtocol._
import appleProtocol._

scala>  toJsonString(Apple("honeycrisp"))
res0: String =
{
  "$type": "Apple",
  "kind": "honeycrisp"
}

scala> fromJsonString(res0)
res1: Apple = Apple(honeycrisp)
```

## Other ways of getting Pickling

If you would like to run the latest development version of scala/pickling (0.10.2-SNAPSHOT), you also need to add the Sonatype "snapshots" repository resolver to your build file:

```scala
libraryDependencies += "org.scala-lang.modules" %% "scala-pickling" % "0.10.2-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")
```

For a more illustrative example, see a [sample sbt project which uses Scala Pickling](https://github.com/xeno-by/sbt-example-pickling).

Or you can just directly download the 0.10.1 jar ([Scala 2.10](https://oss.sonatype.org/service/local/artifact/maven/redirect?r=releases&g=org.scala-lang.modules&a=scala-pickling_2.10&v=0.10.1&e=jar), [Scala 2.11](https://oss.sonatype.org/service/local/artifact/maven/redirect?r=releases&g=org.scala-lang.modules&a=scala-pickling_2.11&v=0.10.1&e=jar)).

