package scala.pickling.fastbinary

import java.io.OutputStream
import scala.pickling.ArrayOutput


object FastArrayOutput {

  private[pickling] val arrayBuilder = new ThreadLocal[ByteArrayBuilder] {
    // initially a builder with pre-allocated array
    override def initialValue(): ByteArrayBuilder = new ByteArray(64 * 1024 * 1024) // 64 MB
  }

}

class FastArrayOutput extends ArrayOutput[Byte] {

  private var builder: ByteArrayBuilder = {
    val b = FastArrayOutput.arrayBuilder.get()
    val reuse = b.checkpoint()
    if (!reuse) {
      // allocate new array builder
      val newb = new ByteArray(64 * 1024 * 1024) // 64 MB
      // put into thread-local storage
      FastArrayOutput.arrayBuilder.set(newb)
      newb.checkpoint() // guaranteed to return `true`
      newb
    } else b
  }

  // replace `builder` with a growable one
  private def replaceBuilder(): Unit = {
    builder = new ByteArrayBuffer(builder.toArray)
    FastArrayOutput.arrayBuilder.set(builder)
  }

  def result(): Array[Byte] = builder.toArray

  def +=(obj: Byte) = {
    // if builder is full replace `builder` with a growable one
    if (builder.isFull) replaceBuilder()
    builder += obj
  }

  def put(obj: Array[Byte]): this.type = {
    // should use `target`
    throw new java.lang.IllegalStateException
  }

  override def target(l: Int) = try {
    builder.target(l)
  } catch {
    case e: IllegalArgumentException =>
      // `builder` does not have enough space
      replaceBuilder()
      builder.target(l) // guaranteed to succeed
  }

  override def flush(a: Array[Byte]) =
    builder.flush(a)
}


final class FastOutputStreamOutput(out: OutputStream) extends FastArrayOutput {
  override def result(): Array[Byte] =
    null

  override def +=(obj: Byte) =
    out.write(obj.asInstanceOf[Int])

  override def put(obj: Array[Byte]): this.type = {
    out.write(obj)
    this
  }

  override def target(len: Int): (Array[Byte], Int) =
    (Array.ofDim[Byte](len), 0)

  override def flush(arr: Array[Byte]): Unit =
    this.put(arr)
}
