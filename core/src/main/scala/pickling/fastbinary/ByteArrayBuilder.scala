package scala.pickling.fastbinary

import scala.pickling.binary.UnsafeMemory


private[fastbinary] sealed abstract class ByteArrayBuilder {

  /** Tests whether this instance is full and must be replaced
   *  by a new (typically growable) instance.
   */
  def isFull: Boolean

  /** Contract: must have called `isFull`
   */
  def +=(b: Byte): Unit

  def toArray: Array[Byte]

  def target(l: Int): (Array[Byte], Int)

  def flush(a: Array[Byte]): Unit

  /** Checkpoints the current state of the builder, so that
   *  its resources can be reused.
   *
   *  @return  `true` if `this` builder is ready to be reused
   */
  def checkpoint(): Boolean
}

private[fastbinary] final class ByteArray(len: Int) extends ByteArrayBuilder {
  // TODO: use Unsafe for allocation?
  private val arr = Array.ofDim[Byte](len)
  private var pos = 0
  private var startPos = 0 // invariant: startPos <= pos

  def isFull: Boolean =
    pos == len

  def +=(b: Byte): Unit = {
    UnsafeMemory.putInt(arr, pos, b)
    pos += 1
  }

  def toArray: Array[Byte] = {
    val numBytes = pos - startPos
    val newArray = Array.ofDim[Byte](numBytes)
    // fast array copy
    val offset = UnsafeMemory.byteArrayOffset
    UnsafeMemory.unsafe.copyMemory(arr, offset + startPos, newArray, offset, numBytes)
    newArray
  }

  def target(l: Int): (Array[Byte], Int) = {
    if (pos + l > len)
      throw new IllegalArgumentException
    val oldpos = pos
    pos = pos + l
    (arr, oldpos)
  }

  def flush(a: Array[Byte]): Unit = { /* no-op */ }

  def checkpoint(): Boolean = {
    startPos = pos
    true
  }
}

private[fastbinary] final class ByteArrayBuffer(headArray: Array[Byte]) extends ByteArrayBuilder {

  private val headLength = headArray.length

  // size of newly created array chunks
  private val chunkSize = 64

  private def mkNode(): Node = new Node(Array.ofDim[Byte](chunkSize), null)

  private var lastNode = mkNode()

  private val headNode = new Node(headArray, lastNode)

  // position in array that is currently being filled (*not* array of `headNode`)
  private var pos = 0

  // cached size of entire builder; is never reset (unlike `pos`)
  private var size0 = headLength

  def isFull: Boolean = false // growable

  def +=(b: Byte): Unit = if (pos < chunkSize) {
    // array in `lastNode` has space
    lastNode.arr(pos) = b
    size0 += 1
    pos += 1
  } else { // pos == chunkSize
    val newNode = mkNode()
    lastNode.next = newNode
    lastNode = newNode
    pos = 0
    // try again
    this += b
  }

  def toArray: Array[Byte] = {
    val newArray = Array.ofDim[Byte](size0)

    // copy `headArray` to `newArray`
    System.arraycopy(headArray, 0, newArray, 0, headLength)

    // copy subsequent arrays (if any)
    var destPos = headLength
    var n = headNode.next
    while (n != null) {
      // decide how many bytes to copy (`pos` many or `chunkSize` many)
      val numToCopy = if (n.next != null) chunkSize else pos
      System.arraycopy(n.arr, 0, newArray, destPos, numToCopy)
      destPos += numToCopy
      n = n.next
    }

    newArray
  }

  def target(l: Int): (Array[Byte], Int) =
    (Array.ofDim[Byte](l), 0)

  def flush(a: Array[Byte]): Unit = {
    var i = 0
    while (i < a.length) {
      this += a(i)
      i += 1
    }
  }

  def checkpoint(): Boolean = false
}

private[fastbinary] final class Node(val arr: Array[Byte], var next: Node)
