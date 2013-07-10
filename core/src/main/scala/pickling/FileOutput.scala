package scala.pickling
package io

import java.io.{File, PrintWriter}

class TextFileOutput(file: File) extends Output[String] {

  private val writer = new PrintWriter(file)

  def result(): String =
    ???

  def put(obj: String): this.type = {
    writer.print(obj)
    this
  }

  def close(): Unit = writer.close()

}
