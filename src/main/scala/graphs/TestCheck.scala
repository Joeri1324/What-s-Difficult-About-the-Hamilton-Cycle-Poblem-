package graphs
import scala.collection._

object TestCheck extends App {

  val graph = Array(
    Array(0, 1, 1, 0),
    Array(1, 0, 1, 0),
    Array(1, 1, 0, 0),
    Array(0, 0, 0, 0)
  )

  val edgeMap = ArbitraryHeuristic.createEdgeMap(graph)

  println(edgeMap)

  println(ArbitraryHeuristic.checkClosedLoop(edgeMap))
}
