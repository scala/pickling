#!/bin/sh
SCRIPT="$(cd "${0%/*}" 2>/dev/null; echo "$PWD"/"${0##*/}")"
DIR=`dirname "${SCRIPT}"}`
exec scala $0 $DIR $SCRIPT "$@"
::!#

import java.io.File
import scala.sys.process._

object App {
  def main(args: Array[String]): Unit = {
    println("args: " + args.mkString(","))
    //val Array(directory,script) = args.map(new File(_).getAbsolutePath)

    // args[0], start: Int
    // args[1], finish: Int
    // args[3], increment: Int
    // args[4], numRuns: Int

    val start:  Int = args(2).toInt
    val finish: Int = args(3).toInt
    val increment: Int = args(4).toInt
    val numRuns = if (args.length >= 6) args(5).toInt
    else 10

    for (len <- (start to finish by increment)) {
      val runBench: String = "scala -Dsize="+ len +" -cp classes ListIntGeneratedBench " + numRuns
      runBench.!
    }
  }
}
