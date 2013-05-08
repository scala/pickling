package scala.pickling

import scala.reflect.runtime.universe._

class Tuple2Pickler[T1: FastTypeTag, T2: FastTypeTag]
    (implicit pickler1: Pickler[T1], unpickler1: Unpickler[T1],
              pickler2: Pickler[T2], unpickler2: Unpickler[T2],
              pf: PickleFormat, tupleTag: FastTypeTag[(T1, T2)])
    extends Pickler[(T1, T2)] with Unpickler[(T1, T2)] {

  val format: PickleFormat = pf

  def pickle(tuple: (T1, T2), builder: PickleBuilder): Unit = {
    builder.hintTag(tupleTag)
    builder.beginEntry(tuple)

    val tag1 = fastTypeTag[T1]
    val sym1 = tag1.tpe.typeSymbol.asClass
    if (sym1.isEffectivelyFinal) builder.hintStaticallyElidedType()
    builder.hintTag(tag1)
    pickler1.pickle(tuple._1, builder)

    val tag2 = fastTypeTag[T2]
    val sym2 = tag2.tpe.typeSymbol.asClass
    if (sym2.isEffectivelyFinal) builder.hintStaticallyElidedType()
    builder.hintTag(tag2)
    pickler2.pickle(tuple._2, builder)

    builder.endEntry()
  }

  def unpickle(tpe: => FastTypeTag[_], preader: PickleReader): Any = {
    val tag = preader.beginEntry()

    val tag1 = preader.beginEntry()
    val val1 = unpickler1.unpickle(tag1, preader)
    preader.endEntry()

    val tag2 = preader.beginEntry()
    val val2 = unpickler2.unpickle(tag2, preader)
    preader.endEntry()

    preader.endEntry()
    (val1, val2)
  }
}

class Tuple3Pickler[T1: FastTypeTag, T2: FastTypeTag, T3: FastTypeTag]
    (implicit pickler1: Pickler[T1], unpickler1: Unpickler[T1],
              pickler2: Pickler[T2], unpickler2: Unpickler[T2],
              pickler3: Pickler[T3], unpickler3: Unpickler[T3],
              pf: PickleFormat, tupleTag: FastTypeTag[(T1, T2, T3)])
    extends Pickler[(T1, T2, T3)] with Unpickler[(T1, T2, T3)] {

  val format: PickleFormat = pf

  def pickle(tuple: (T1, T2, T3), builder: PickleBuilder): Unit = {
    builder.hintTag(tupleTag)
    builder.beginEntry(tuple)

    val tag1 = fastTypeTag[T1]
    val sym1 = tag1.tpe.typeSymbol.asClass
    if (sym1.isEffectivelyFinal) builder.hintStaticallyElidedType()
    builder.hintTag(tag1)
    pickler1.pickle(tuple._1, builder)

    val tag2 = fastTypeTag[T2]
    val sym2 = tag2.tpe.typeSymbol.asClass
    if (sym2.isEffectivelyFinal) builder.hintStaticallyElidedType()
    builder.hintTag(tag2)
    pickler2.pickle(tuple._2, builder)

    val tag3 = fastTypeTag[T3]
    val sym3 = tag3.tpe.typeSymbol.asClass
    if (sym3.isEffectivelyFinal) builder.hintStaticallyElidedType()
    builder.hintTag(tag3)
    pickler3.pickle(tuple._3, builder)

    builder.endEntry()
  }

  def unpickle(tpe: => FastTypeTag[_], preader: PickleReader): Any = {
    val tag = preader.beginEntry()

    val tag1 = preader.beginEntry()
    val val1 = unpickler1.unpickle(tag1, preader)
    preader.endEntry()

    val tag2 = preader.beginEntry()
    val val2 = unpickler2.unpickle(tag2, preader)
    preader.endEntry()

    val tag3 = preader.beginEntry()
    val val3 = unpickler3.unpickle(tag3, preader)
    preader.endEntry()

    preader.endEntry()
    (val1, val2, val3)
  }
}
