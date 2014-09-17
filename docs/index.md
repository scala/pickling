<p>&nbsp;</p>
<!-- <p>&nbsp;</p> -->

Scala/pickling is an automatic serialization framework for Scala. It's fast,
boilerplate-free, and allows users to easily swap in/out different
serialization formats (such as binary, or JSON), or even to provide your own
custom serialization format.

**Basic usage:**
~~~scala
import scala.pickling._
import json._

val pckl = List(1, 2, 3, 4).pickle
val lst = pckl.unpickle[List[Int]]
~~~

# Getting Started

## Installation