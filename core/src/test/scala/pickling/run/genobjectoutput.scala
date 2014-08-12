package scala.pickling.test

import org.scalatest.FunSuite
import scala.pickling.util.Externalizables
import java.io.{Externalizable, ObjectInput, ObjectOutput}


class ExtData private (private var b: Byte,
                       private var i: Int,
                       private var c: Char,
                       private var j: Int) extends Externalizable {

  def writeExternal(out: ObjectOutput) {
    out.writeByte(b)
    out.writeInt(i)
    out.writeChar(c)
    out.writeInt(j)
  }

  def readExternal(in: ObjectInput) {
    b = in.readByte()
    i = in.readInt()
    c = in.readChar()
    j = in.readInt()
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[ExtData] && {
      val o = other.asInstanceOf[ExtData]
      o.b == b && o.i == i && o.c == c && o.j == j
    }
}

object ExtData {
  def apply(b: Byte, i: Int, c: Char, j: Int): ExtData =
    new ExtData(b, i, c, j)
}

class ExtData2 private (private var b: Byte,
                        private var i: Int,
                        private var bytes: Array[Byte],
                        private var j: Int) extends Externalizable {

  def writeExternal(out: ObjectOutput) {
    out.writeByte(b)
    out.writeInt(i)
    out.write(bytes)
    out.writeInt(j)
  }

  def readExternal(in: ObjectInput) {
    b = in.readByte()
    i = in.readInt()
    bytes = new Array[Byte](i)
    in.readFully(bytes)
    j = in.readInt()
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[ExtData2] && {
      val o = other.asInstanceOf[ExtData2]
      o.b == b && o.i == i && o.bytes.mkString == bytes.mkString && o.j == j
    }

  override def toString: String =
    s"ExtData2($b,$i,${bytes.mkString},$j)"
}

object ExtData2 {
  def apply(b: Byte, i: Int, bytes: Array[Byte], j: Int): ExtData2 =
    new ExtData2(b, i, bytes, j)
}

class ExtDataComp(private val x: Int) {
  override def equals(o: Any) =
    o.isInstanceOf[ExtDataComp] && o.asInstanceOf[ExtDataComp].x == x
}

class ExtData3 private (private var b: Byte,
                        private var i: Int,
                        private var bytes: Array[Byte],
                        private var j: ExtDataComp) extends Externalizable {

  def writeExternal(out: ObjectOutput) {
    out.writeByte(b)
    out.writeInt(i)
    out.write(bytes)
    out.writeObject(j)
  }

  def readExternal(in: ObjectInput) {
    b = in.readByte()
    i = in.readInt()
    bytes = new Array[Byte](i)
    in.readFully(bytes)
    j = in.readObject().asInstanceOf[ExtDataComp]
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[ExtData3] && {
      val o = other.asInstanceOf[ExtData3]
      o.b == b && o.i == i && o.bytes.mkString == bytes.mkString && o.j == j
    }

  override def toString: String =
    s"ExtData2($b,$i,${bytes.mkString},$j)"
}

object ExtData3 {
  def apply(b: Byte, i: Int, bytes: Array[Byte], j: ExtDataComp): ExtData3 =
    new ExtData3(b, i, bytes, j)
}


class GenObjectOutputTest extends FunSuite {
  test("Externalizable") {
    val data  = ExtData(10, 0, 'z', 1)
    val out   = Externalizables.genOutput[(Byte, Int, Char, Int)]
    data.writeExternal(out)

    val data2 = ExtData(0, 0, 'a', 0)
    val in    = Externalizables.genInput[(Byte, Int, Char, Int)]((
      out.byteArr(0).asInstanceOf[Byte],
      out.intArr(0),
      out.charArr(0).asInstanceOf[Char],
      out.intArr(1)))
    data2.readExternal(in)

    assert(data2 == data)
  }

  test("Externalizable Array[Byte]") {
    val data  = ExtData2(10, 3, Array[Byte](1, 2, 30), 1)
    val out   = Externalizables.genOutput[(Byte, Int, Array[Byte], Int)]
    data.writeExternal(out)

    val data2 = ExtData2(0, 0, Array[Byte](), 0)
    val in    = Externalizables.genInput[(Byte, Int, Array[Byte], Int)]((
      out.byteArr(0).asInstanceOf[Byte],
      out.intArr(0),
      out.arrByteArr(0),
      out.intArr(1)))
    data2.readExternal(in)

    assert(data2 == data)
  }

  test("Externalizable AnyRef") {
    val data  = ExtData3(10, 3, Array[Byte](1, 2, 30), new ExtDataComp(5))
    val out   = Externalizables.genOutput[(Byte, Int, Array[Byte], AnyRef)]
    data.writeExternal(out)

    val data2 = ExtData3(0, 0, Array[Byte](), new ExtDataComp(0))
    val in    = Externalizables.genInput[(Byte, Int, Array[Byte], AnyRef)]((
      out.byteArr(0).asInstanceOf[Byte],
      out.intArr(0),
      out.arrByteArr(0),
      out.anyRefArr(0).asInstanceOf[AnyRef]))
    data2.readExternal(in)

    assert(data2 == data)
  }
}
