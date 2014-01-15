package scala.pickling

trait Output[T] {

  def result(): T

  def put(obj: T): this.type

}

// and then demand Output[Nothing] in the abstract PickleFormat
// in JSON we can demand Output[String], since Output[Nothing] <: Output[String]

import scala.reflect.ClassTag
// Array output with a few more methods for performance
abstract class ArrayOutput[T: ClassTag] extends Output[Array[T]] {
  // Put a single T
  def +=(obj: T): Unit
  // Allocate a new array.
  def target(len: Int): (Array[T], Int) =
    (Array.ofDim[T](len), 0)
  // Flush the allocated array by target().
  def flush(arr: Array[T]): Unit =
    this.put(arr)
}

import scala.collection.mutable.ArrayBuffer

class ByteArrayBufferOutput extends ArrayOutput[Byte] {

  private val buf =
    ArrayBuffer[Byte]()
    
  def result(): Array[Byte] =
    buf.toArray
  
  def +=(obj: Byte) =
    buf += obj
  
  def put(obj: Array[Byte]): this.type = {
    buf ++= obj
    this
  }
}

class ByteArrayOutput(len: Int) extends ArrayOutput[Byte]  {

  private var pos = 0
  private val arr = Array.ofDim[Byte](len)
  
  def result(): Array[Byte] =
    arr
  
  def +=(obj: Byte) = {
    arr(pos) = obj
    pos = pos + 1
  }
  
  def put(obj: Array[Byte]): this.type = {
	// target() should be used to avoid double copy
    throw new java.lang.IllegalStateException
  }
  
  override def target(len: Int) = {
    val oldpos = pos
    pos = pos + len
    (arr, oldpos)
	}
  
  override def flush(arr: Array[Byte]) = { /*noop*/ }
}

class StringOutput extends Output[String] {

  private val buf =
    new StringBuilder()

  def result(): String =
    buf.toString

  def put(obj: String): this.type = {
    buf ++= obj
    this
  }

  override def toString = buf.toString

}
