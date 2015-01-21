package scala.pickling.externalizable

import org.scalatest.FunSuite
import scala.pickling._, scala.pickling.Defaults._
import java.io.{Externalizable, IOException, ObjectInput, ObjectOutput}
import java.nio.ByteBuffer

class StorageLevel private(
    private var useDisk_ : Boolean,
    private var useMemory_ : Boolean,
    private var deserialized_ : Boolean,
    @transient private var buf: ByteBuffer,
    private var replication_ : Int)
  extends Externalizable {

  private def this(x: Boolean, y: Boolean, z: Boolean, r: Int = 1) = {
    this(x, y, z, ByteBuffer.wrap(Array[Byte](1, 2, 3)), r)
  }

  // TODO: Also add fields for caching priority, dataset ID, and flushing.
  private def this(flags: Int, replication: Int) {
    this((flags & 4) != 0, (flags & 2) != 0, (flags & 1) != 0, replication)
  }

  def this() = this(false, true, false)  // For deserialization

  def useDisk = useDisk_
  def useMemory = useMemory_
  def deserialized = deserialized_
  def replication = replication_

  override def clone(): StorageLevel = new StorageLevel(
    this.useDisk, this.useMemory, this.deserialized, this.replication)

  override def equals(other: Any): Boolean = other match {
    case s: StorageLevel =>
      s.useDisk == useDisk &&
      s.useMemory == useMemory &&
      s.deserialized == deserialized &&
      s.replication == replication
    case _ =>
      false
  }

  override def toString: String =
    "StorageLevel(%b, %b, %b, %d)".format(useDisk, useMemory, deserialized, replication)

  def toInt: Int = {
    var ret = 0
    if (useDisk_) {
      ret |= 4
    }
    if (useMemory_) {
      ret |= 2
    }
    if (deserialized_) {
      ret |= 1
    }
    ret
  }

  override def writeExternal(out: ObjectOutput) {
    out.writeByte(toInt)
    out.writeByte(replication_)
  }

  override def readExternal(in: ObjectInput) {
    val flags = in.readByte()
    useDisk_ = (flags & 4) != 0
    useMemory_ = (flags & 2) != 0
    deserialized_ = (flags & 1) != 0
    replication_ = in.readByte()
  }
}

object StorageLevel {
  val NONE = new StorageLevel(false, false, false)
  val DISK_ONLY = new StorageLevel(true, false, false)
  val DISK_ONLY_2 = new StorageLevel(true, false, false, 2)
  val MEMORY_ONLY = new StorageLevel(false, true, true)
  val MEMORY_ONLY_2 = new StorageLevel(false, true, true, 2)
  val MEMORY_ONLY_SER = new StorageLevel(false, true, false)
  val MEMORY_ONLY_SER_2 = new StorageLevel(false, true, false, 2)
  val MEMORY_AND_DISK = new StorageLevel(true, true, true)
  val MEMORY_AND_DISK_2 = new StorageLevel(true, true, true, 2)
  val MEMORY_AND_DISK_SER = new StorageLevel(true, true, false)
  val MEMORY_AND_DISK_SER_2 = new StorageLevel(true, true, false, 2)

  /** Create a new StorageLevel object */
  def apply(useDisk: Boolean, useMemory: Boolean, deserialized: Boolean, replication: Int = 1) =
    getCachedStorageLevel(new StorageLevel(useDisk, useMemory, deserialized, replication))

  /** Create a new StorageLevel object from its integer representation */
  def apply(flags: Int, replication: Int) =
    getCachedStorageLevel(new StorageLevel(flags, replication))

  /** Read StorageLevel object from ObjectInput stream */
  def apply(in: ObjectInput) = {
    val obj = new StorageLevel()
    obj.readExternal(in)
    getCachedStorageLevel(obj)
  }

  private
  val storageLevelCache = new java.util.concurrent.ConcurrentHashMap[StorageLevel, StorageLevel]()

  private def getCachedStorageLevel(level: StorageLevel): StorageLevel = {
    storageLevelCache.putIfAbsent(level, level)
    storageLevelCache.get(level)
  }
}

class StorageLevel2 (
    private var useDisk_ : Boolean,
    private var useMemory_ : Boolean,
    private var deserialized_ : Boolean,
    private var replication_ : Int)
  extends Externalizable {

  def toInt: Int = {
    var ret = 0
    if (useDisk_) {
      ret |= 4
    }
    if (useMemory_) {
      ret |= 2
    }
    if (deserialized_) {
      ret |= 1
    }
    ret
  }

  override def writeExternal(out: ObjectOutput) {
    out.writeByte(toInt)
    out.writeByte(replication_)
  }

  override def readExternal(in: ObjectInput) {
    val flags = in.readByte()
    useDisk_ = (flags & 4) != 0
    useMemory_ = (flags & 2) != 0
    deserialized_ = (flags & 1) != 0
    replication_ = in.readByte()
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[StorageLevel2] && {
      val o = other.asInstanceOf[StorageLevel2]
      o.useDisk_ == useDisk_ && o.useMemory_ == useMemory_ && o.deserialized_ == deserialized_ && o.replication_ == replication_
    }
}

class StorageLevel3(private var s: String, private var x: Int) extends Externalizable {
  override def writeExternal(out: ObjectOutput) {
    out.writeUTF(s)
    out.writeInt(x)
  }

  override def readExternal(in: ObjectInput) {
    s = in.readUTF()
    x = in.readInt()
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[StorageLevel3] && {
      val o = other.asInstanceOf[StorageLevel3]
      o.s == s && o.x == x
    }
}

class ExternalizableTest extends FunSuite {
  test("main") {
    import json._

    val sl = StorageLevel.MEMORY_ONLY_SER
    val pickle: JSONPickle = sl.pickle
    val up = pickle.unpickle[StorageLevel]
    assert(sl == up)
  }

  test("Externalizable pickler must not use knownSize") {
    import binary._

    val obj = new StorageLevel2(false, true, false, 1)
    val p = obj.pickle
    val up = p.unpickle[StorageLevel2]
    assert(up == obj)
  }

  test("writeUTF/readUTF") {
    import json._

    val obj = new StorageLevel3("test", 5)
    val pickle: JSONPickle = obj.pickle
    val up = pickle.unpickle[StorageLevel3]
    assert(up == obj)
  }
}
