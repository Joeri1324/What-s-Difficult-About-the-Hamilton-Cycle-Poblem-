package graphs

import org.scalatest._
import scala.collection._

class MartelloSpec extends FlatSpec with Matchers {

  "Implied edges" should "contain correct edges" in {
    val out = Map(
      1 -> Set(2, 3),
      2 -> Set(1, 3),
      3 -> Set(4, 1, 2),
      4 -> Set(1)
    )
    val in = Map(
      1 -> Set(4, 2, 3),
      2 -> Set(1, 3),
      3 -> Set(1, 2),
      4 -> Set(3)
    )
    val implied = List((4, 1), (3, 4))
    Martello.getImplied(in, out).toSet shouldEqual implied.toSet
  }

  "PrunePath" should "return ends of one degree path" in {
    val out = Map(
      2 -> Set(3),
      1 -> Set(3),
      3 -> Set(4, 5),
      4 -> Set(2, 1),
      5 -> Set[Int]()
    )
    val in = Map(
      1 -> Set(4),
      3 -> Set(1, 2),
      2 -> Set(4),
      4 -> Set(3),
      5 -> Set(3)
    )

    val implied         = Martello.getImplied(in, out)
    val shouldBeDeleted = List((3, 4))
    Martello.prunePath(in, out, implied) shouldEqual shouldBeDeleted
  }

  "PruneEnds" should "return ends of implied arcs" in {
    val out = Map(
      1 -> Set(2),
      2 -> Set(3),
      3 -> Set(4, 2),
      4 -> Set[Int]()
    )
    val in = Map(
      1 -> Set[Int](),
      2 -> Set(1, 3),
      3 -> Set(2),
      4 -> Set(3)
    )


    val implied = Martello.getImplied(in, out)
    Martello.pruneEnds(implied, in, out, 1) shouldEqual Set((3, 2))
  }
}