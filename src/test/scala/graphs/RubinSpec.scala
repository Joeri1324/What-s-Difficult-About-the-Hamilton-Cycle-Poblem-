package graphs

import org.scalatest._

class RubinSpec extends FlatSpec with Matchers {
  "Rubin" should "correctly put directed in required" in {
    val directedIn = Map(
      0 -> Set(1),
      1 -> Set(2),
      2 -> Set(3),
      3 -> Set[Int]()
    )
    val directedOut = Map(
      0 -> Set[Int](),
      1 -> Set(0),
      2 -> Set(1),
      3 -> Set(2)
    )
    val r = Rubin2.getRequiredDirected(directedIn, directedOut)

    r shouldEqual Vector((0, 1), (1, 2), (2, 3))
  }

  "Rubin" should "correctly put undirected in required" in {
    val undirected = Map(
      0 -> Set(1, 2),
      1 -> Set(0, 2),
      2 -> Set(0, 1, 3),
      3 -> Set[Int]()
    )
    val r = Rubin2.deleteUndirected(undirected)

    println("chicken", r)
  }

  "Rubin" should "correctly delete directed" in {
    val directedIn = Map(
      0 -> Set(1),
      3 -> Set[Int](),
      1 -> Set[Int](),
      5 -> Set(6, 7),
      6 -> Set[Int](),
      7 -> Set[Int]()
    )
    val directedOut = Map(
      1 -> Set(0, 3),
      3 -> Set[Int](),
      5 -> Set[Int](),
      4 -> Set(5),
      6 -> Set(5),
      7 -> Set(5)
    )
    println(Rubin2.deleteDirected(directedIn, directedOut))
  }
}
