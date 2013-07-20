package scala.pickling

import scala.reflect.ClassTag

class ReactMap {
  val emptyValNil = -1

  private var keytable: Array[AnyRef]   = new Array[AnyRef](ReactMap.initSize)
  private var valtable: Array[Int] = Array.fill[Int](ReactMap.initSize)(Int.MinValue)

  private var sz = 0

  def lookup(k: AnyRef): Int = {
    var pos = index(k)
    val nil = null
    var curr = keytable(pos)

    while (curr != nil && curr != k) {
      pos = (pos + 1) % keytable.length
      curr = keytable(pos)
    }

    if (curr == nil) emptyValNil
    else valtable(pos)
  }

  def insert(k: AnyRef, v: Int): Int = {
    checkResize()

    var pos = index(k)
    val nil = null
    var curr = keytable(pos)
    assert(k != nil)

    while (curr != nil && curr != k) {
      pos = (pos + 1) % keytable.length
      curr = keytable(pos)
    }

    val previousValue = valtable(pos)
    keytable(pos) = k
    valtable(pos) = v
    val added = curr == nil
    if (added) sz += 1

    previousValue
  }

  private def delete(k: AnyRef): Int = {
    var pos = index(k)
    val nil = null
    var curr = keytable(pos)

    while (curr != nil && curr != k) {
      pos = (pos + 1) % keytable.length
      curr = keytable(pos)
    }

    if (curr == nil) emptyValNil
    else {
      val previousValue = valtable(pos)

      var h0 = pos
      var h1 = (h0 + 1) % keytable.length
      while (keytable(h1) != nil) {
        val h2 = index(keytable(h1))
        if (h2 != h1 && before(h2, h0)) {
          keytable(h0) = keytable(h1)
          valtable(h0) = valtable(h1)
          h0 = h1
        }
        h1 = (h1 + 1) % keytable.length
      }

      keytable(h0) = null
      valtable(h1) = emptyValNil
      sz -= 1

      previousValue
    }
  }

  private def checkResize() {
    if (sz * 1000 / ReactMap.loadFactor > keytable.length) {
      val okeytable = keytable
      val ovaltable = valtable
      val ncapacity = keytable.length * 2
      keytable = new Array[AnyRef](ncapacity)
      valtable = Array.fill[Int](ncapacity)(emptyValNil)
      sz = 0

      var pos = 0
      val nil = null
      while (pos < okeytable.length) {
        val curr = okeytable(pos)
        if (curr != nil) {
          insert(curr, ovaltable(pos))
        }

        pos += 1
      }
    }
  }

  private def before(i: Int, j: Int) = {
    val d = keytable.length >> 1
    if (i <= j) j - i < d
    else i - j > d
  }

  private def index(k: AnyRef): Int = {
    val hc = k.##
    math.abs(scala.util.hashing.byteswap32(hc)) % keytable.length
  }

  def apply(key: AnyRef): Int = lookup(key) match {
    case `emptyValNil` => throw new NoSuchElementException("key: " + key)
    case v => v
  }

  def get(key: AnyRef): Option[Int] = lookup(key) match {
    case `emptyValNil` => None
    case v => Some(v)
  }

  def contains(key: AnyRef): Boolean = lookup(key) match {
    case `emptyValNil` => false
    case v => true
  }

  def update(key: AnyRef, value: Int): Unit = insert(key, value)

  def remove(key: AnyRef): Boolean = delete(key) match {
    case `emptyValNil` => false
    case v => true
  }

  def clear() {
    var pos = 0
    val nil = null
    while (pos < keytable.length) {
      if (keytable(pos) != nil) {
        val k = keytable(pos)
        val v = valtable(pos)

        keytable(pos) = null
        valtable(pos) = emptyValNil
        sz -= 1
      }

      pos += 1
    }
  }

  def size: Int = sz

}


object ReactMap {

  def apply() = new ReactMap

  val initSize = 1024

  val loadFactor = 450

}
