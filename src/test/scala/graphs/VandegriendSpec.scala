package graphs

import org.scalatest._
import scala.collection._

class VandegriendSpec extends FlatSpec with Matchers {

  "Prune" should "correctly remove triangle with degree 2" in {
    val triangle: Map[Int, mutable.Set[Int]] = mutable.Map(
      0 -> mutable.Set(1, 2),
      1 -> mutable.Set(0, 2),
      2 -> mutable.Set(0, 1, 3),
      3 -> mutable.Set[Int]()
    )
    val afterPrune: Map[Int, mutable.Set[Int]] = mutable.Map(
      0 -> mutable.Set(1, 2),
      1 -> mutable.Set(0, 2),
      2 -> mutable.Set(0, 1),
      3 -> mutable.Set[Int]()
    )
    val deleted = Vandegriend.pruneDegreeTwoNeighbours(triangle)
    triangle shouldEqual afterPrune
    deleted shouldEqual mutable.ListBuffer((2, 3))
  }

  "FuncPrune" should "correctly remove triangle with degree 2" in {
    val triangle: Map[Int, Set[Int]] = Map(
      0 -> Set(1, 2),
      1 -> Set(0, 2),
      2 -> Set(0, 1, 3),
      3 -> Set[Int]()
    )
    val afterPrune: Map[Int, Set[Int]] = Map(
      0 -> Set(1, 2),
      1 -> Set(0, 2),
      2 -> Set(0, 1),
      3 -> Set[Int]()
    )
    val deleted = Vandegriend.funcPruneDegreeTwoNeighbours(triangle)
    deleted shouldEqual afterPrune
    // deleted shouldEqual mutable.List((2, 3))
  }

  "FuncPrune" should "correctly ends of paths with degree 2" in {
    val graph: Map[Int, Set[Int]] = Map(
      0 -> Set(1, 2, 3),
      1 -> Set(0, 2),
      2 -> Set(0, 1, 3),
      3 -> Set[Int]()
    )
    val afterPrune: Map[Int, Set[Int]] = Map(
      0 -> Set(1, 3),
      1 -> Set(0, 2),
      2 -> Set(1, 3),
      3 -> Set[Int]()
    )
    //println(Vandegriend.funcPruneTriangleWithTwoDegreePath(graph))
  }
}
