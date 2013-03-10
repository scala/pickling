import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

class QuasiquoteProperties(name: String) extends Properties(name)
                                            with ArbitraryTreesAndNames {

  def test(name: String)(block: => Boolean): Unit =
    property(name) = exists { (u: Unit) => block }

  implicit class TestSimilarTree(tree1: Tree) {
    def ≈(tree2: Tree) = tree1.equalsStructure(tree2)
  }

  implicit class TestSimilarListTree(lst: List[Tree]) {
    def ≈(other: List[Tree]) = (lst.length == other.length) && lst.zip(other).forall { case (t1, t2) => t1 ≈ t2 }
  }

  implicit class TestSimilarListListTree(lst: List[List[Tree]]) {
    def ≈(other: List[List[Tree]]) = (lst.length == other.length) && lst.zip(other).forall { case (l1, l2) => l1 ≈ l2 }
  }
}