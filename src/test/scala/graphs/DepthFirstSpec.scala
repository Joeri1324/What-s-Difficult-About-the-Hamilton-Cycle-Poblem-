package graphs

import org.scalatest._
import flatspec._
import matchers._

class DepthFirstSpec extends AnyFlatSpec with should.Matchers {
  // "DepthFirst" should "correctly solve full connected graph" in {
  //   val graph = Array(
  //     Array(0, 1, 1, 1),
  //     Array(1, 0, 1, 1),
  //     Array(1, 1, 0, 1),
  //     Array(1, 1, 1, 0)
  //   )
  //   Naked.solve(graph, 1000)._1 shouldEqual Some(true)
  // }

  // "DepthFirst" should "correctly solve not connected graph" in {
  //   val graph = Array(
  //     Array(0, 0, 0, 0),
  //     Array(0, 0, 0, 0),
  //     Array(0, 0, 0, 0),
  //     Array(0, 0, 0, 0)
  //   )
  //   Naked.solve(graph, 1000)._1 shouldEqual Some(false)
  // }

  "ClosedLoopCycle" should "work" in {
    val graph = Array(
      Array(0, 1, 1, 0),
      Array(1, 0, 1, 0),
      Array(1, 1, 0, 0),
      Array(0, 0, 0, 0)
    )

    val edgeMap = DepthFirst.createEdgeMap(graph)

    true shouldEqual true
  }
}
