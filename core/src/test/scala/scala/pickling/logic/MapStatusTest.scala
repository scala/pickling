/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scala.pickling.externalizable.mapstatus

import org.scalatest.FunSuite

import scala.pickling._, scala.pickling.Defaults._, json._

import scala.reflect.{ClassTag, classTag}

import java.io.{Externalizable, IOException, ObjectInput, ObjectOutput}
import java.util.concurrent.ConcurrentHashMap

/**
 * This class represent an unique identifier for a BlockManager.
 * The first 2 constructors of this class is made private to ensure that
 * BlockManagerId objects can be created only using the apply method in
 * the companion object. This allows de-duplication of ID objects.
 * Also, constructor parameters are private to ensure that parameters cannot
 * be modified from outside this class.
 */
class BlockManagerId private (
    private var executorId_ : String,
    private var host_ : String,
    private var port_ : Int,
    private var nettyPort_ : Int
  ) extends Externalizable {

  private def this() = this(null, null, 0, 0)  // For deserialization only

  def executorId: String = executorId_

  if (null != host_){
    // Utils.checkHost(host_, "Expected hostname")
    assert (port_ > 0)
  }

  def hostPort: String = {
    // DEBUG code
    // Utils.checkHost(host)
    assert (port > 0)

    host + ":" + port
  }

  def host: String = host_

  def port: Int = port_

  def nettyPort: Int = nettyPort_

  override def writeExternal(out: ObjectOutput) {
    out.writeUTF(executorId_)
    out.writeUTF(host_)
    out.writeInt(port_)
    out.writeInt(nettyPort_)
  }

  override def readExternal(in: ObjectInput) {
    executorId_ = in.readUTF()
    host_ = in.readUTF()
    port_ = in.readInt()
    nettyPort_ = in.readInt()
  }

  @throws(classOf[IOException])
  private def readResolve(): Object = BlockManagerId.getCachedBlockManagerId(this)

  override def toString = "BlockManagerId(%s, %s, %d, %d)".format(executorId, host, port, nettyPort)

  override def hashCode: Int = (executorId.hashCode * 41 + host.hashCode) * 41 + port + nettyPort

  override def equals(that: Any) = that match {
    case id: BlockManagerId =>
      executorId == id.executorId && port == id.port && host == id.host && nettyPort == id.nettyPort
    case _ =>
      false
  }
}


object BlockManagerId {

  /**
   * Returns a [[org.apache.spark.storage.BlockManagerId]] for the given configuraiton.
   *
   * @param execId ID of the executor.
   * @param host Host name of the block manager.
   * @param port Port of the block manager.
   * @param nettyPort Optional port for the Netty-based shuffle sender.
   * @return A new [[org.apache.spark.storage.BlockManagerId]].
   */
  def apply(execId: String, host: String, port: Int, nettyPort: Int) =
    getCachedBlockManagerId(new BlockManagerId(execId, host, port, nettyPort))

  def apply(in: ObjectInput) = {
    val obj = new BlockManagerId()
    obj.readExternal(in)
    getCachedBlockManagerId(obj)
  }

  val blockManagerIdCache = new ConcurrentHashMap[BlockManagerId, BlockManagerId]()

  def getCachedBlockManagerId(id: BlockManagerId): BlockManagerId = {
    blockManagerIdCache.putIfAbsent(id, id)
    blockManagerIdCache.get(id)
  }
}

/**
 * Result returned by a ShuffleMapTask to a scheduler. Includes the block manager address that the
 * task ran on as well as the sizes of outputs for each reducer, for passing on to the reduce tasks.
 * The map output sizes are compressed using MapOutputTracker.compressSize.
 */
class MapStatus(var location: BlockManagerId, var compressedSizes: Array[Byte])
  extends Externalizable {

  def this() = this(null, null)  // For deserialization only

  def writeExternal(out: ObjectOutput) {
    location.writeExternal(out)
    val len = compressedSizes.length
    out.writeInt(len)
    out.write(compressedSizes)
  }

  def readExternal(in: ObjectInput) {
    location = BlockManagerId(in)
    val len = in.readInt()
    compressedSizes = new Array[Byte](len)
    in.readFully(compressedSizes)
  }
}

class MapStatusTest extends FunSuite {
  def register[T: ClassTag: Pickler: Unpickler : FastTypeTag](): Unit = {
    val clazz = classTag[T].runtimeClass
    val p = implicitly[Pickler[T]]
    val up = implicitly[Unpickler[T]]
    val tagKey = implicitly[FastTypeTag[T]].key
    internal.currentRuntime.picklers.registerPickler(tagKey, p)
    internal.currentRuntime.picklers.registerUnpickler(tagKey, up)
  }

  register[MapStatus]

  test("main") {
    val bid = BlockManagerId("0", "localhost", 8080, 8090)
    val ms  = new MapStatus(bid, Array[Byte](1, 2, 3, 4))
    val sizes: String = ms.compressedSizes.mkString(",")

    val p = (ms: Any).pickle
    val up = p.unpickle[Any]

    val ms2 = up.asInstanceOf[MapStatus]
    val sizes2: String = ms2.compressedSizes.mkString(",")
    assert(sizes == sizes2, "same array expected")
  }
}
