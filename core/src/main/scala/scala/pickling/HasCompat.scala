package scala.pickling

// provides a source compatibility stub
// in Scala 2.10.x, it will make `import compat._` compile just fine,
// even though `c.universe` doesn't have `compat`
// in Scala 2.11.0, it will be ignored, becase `import c.universe._`
// brings its own `compat` in scope and that one takes precedence
private object HasCompat { val compat = ??? }
