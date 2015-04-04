package scala.pickling

import scala.pickling.pickler.AllPicklers

/** Import `scala.pickling.Defaults._` to introduce all picklers and ops.
 */
object Defaults extends Ops with AllPicklers {}
