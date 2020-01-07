package graphs

import System._

object Bagel extends App {

    def cutoff(maxTime: Long, maxIter: Int, timeOrIterations: String = "time")
        (curIter: Int, startTime: Long): Boolean =
        if (timeOrIterations == "time") nanoTime - startTime > maxTime
        else                            curIter > maxIter

    val graph = GraphReader.graphsFromFolder("frontend/resources/hillclimb/20 difficult")(0)

    println(CheckAllWithPruningLow.solve(graph.array, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))
}