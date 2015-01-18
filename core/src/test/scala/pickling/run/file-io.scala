package scala.pickling.fileio

import org.scalatest.FunSuite
import java.io.File

import scala.io.Source
import scala.pickling._
import scala.pickling.internal._
import scala.pickling.json._
import scala.pickling.AllPicklers._
import scala.pickling.io.TextFileOutput

case class Person(name: String)
case class PersonNums(name: String, randNums: Array[Int])

class FileIOTest extends FunSuite {
  test("simple") {
    val p = Person("James")

    val tmpFile = File.createTempFile("pickling", "fileoutput")
    val fileOut = new TextFileOutput(tmpFile)

    // We use the implicitly added OPS here to ensure they work.
    p.pickleTo(fileOut)
    fileOut.close()

    val fileContents = Source.fromFile(tmpFile).getLines.mkString("\n")

    assert(fileContents == p.pickle.value)
  }

  test("simple-w-collection") {
    val p = PersonNums("James", (1 to 200).toArray)

    val tmpFile = File.createTempFile("pickling", "fileoutput")
    val fileOut = new TextFileOutput(tmpFile)

    // We use the static method here just to make sure it works
    scala.pickling.pickleTo(json.pickleFormat)(p, fileOut)
    fileOut.close()

    val fileContents = Source.fromFile(tmpFile).getLines.mkString("\n")

    assert(fileContents == p.pickle.value)
  }

  test("simple-w-collection-using-builder-directly") {
    val p = PersonNums("James", (1 to 200).toArray)

    val tmpFile = File.createTempFile("pickling", "fileoutput")
    val fileOut = new TextFileOutput(tmpFile)

    val builder = pickleFormat.createBuilder(fileOut)
    p.pickleInto(builder)
    clearPicklees() // TODO: need something more convenient here
    fileOut.close()

    val fileContents = Source.fromFile(tmpFile).getLines.mkString("\n")

    assert(fileContents == p.pickle.value)
  }
}