package graphs

import System._
// to-do


// Mutate space
object Hillclimb extends App {

  def copy(graph: Array[Array[Int]]): Array[Array[Int]] = {
    return graph.map(_.clone)
  }

  def randomMutation(graph: Array[Array[Int]]) = {
    val newGraph = copy(graph)
    val random = scala.util.Random
    val edges = for (
      i <- graph.indices;
      j <- graph.indices;
      if graph(i)(j) == 1 && i < j 
    ) yield (i, j)
    val complement = for (
      i <- 0 until graph.size;
      j <- 0 until graph.size; 
      if !edges.contains((i, j)) && !edges.contains((j, i)) && i != j && i < j
    ) yield (i, j)

    val deleteEdge = edges(random.nextInt(edges.size))
    val newEdge    = complement(random.nextInt(complement.size))

    // 0 -> replace, 1 -> delete, 2 -> add
    val moveType = random.nextInt(3)

    if (moveType == 0) {
      newGraph(deleteEdge._1)(deleteEdge._2) = 0
      newGraph(deleteEdge._2)(deleteEdge._1) = 0
      newGraph(newEdge._1)(newEdge._2) = 1
      newGraph(newEdge._2)(newEdge._1) = 1
    }
    if (moveType == 1) {
      newGraph(deleteEdge._1)(deleteEdge._2) = 0
      newGraph(deleteEdge._2)(deleteEdge._1) = 0
    }
    if (moveType == 2) {
      newGraph(newEdge._1)(newEdge._2) = 1
      newGraph(newEdge._2)(newEdge._1) = 1
    }

    newGraph
  }

  def cutoff(maxTime: Long, maxIter: Int, timeOrIterations: String = "time")
    (curIter: Int, startTime: Long): Boolean =
    if (timeOrIterations == "time") nanoTime - startTime > maxTime
    else                            curIter > maxIter

  def Hillclimb(maxTries: Int, graph: Array[Array[Int]]): (Array[Array[Int]], Int, Boolean) = {
    var changed = true
    var currentGraph = graph
    var maxFitness = 0
    var maxHamiltonian = true
    var i = 0

    while (i < maxTries) {
      val candidate = randomMutation(currentGraph)
      val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(candidate, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))

      if (recursions > maxFitness) {
        changed = true
        currentGraph = candidate
        maxFitness = recursions
        println("fitness", maxFitness)
        hamiltonian match {
          case Some(true) => { maxHamiltonian = true}
          case Some(false) => { maxHamiltonian = false}
          case None => { maxHamiltonian = false}
        }
      }
      i = i + 1

      if (recursions > 100000) {
        return (currentGraph, maxFitness, maxHamiltonian)
      }

    }
    return (currentGraph, maxFitness, maxHamiltonian)
  }


  val graphSize = 20
  val experiment_times = 10
  val maxEvaluations = 500
  val random = scala.util.Random

  val startingGraph = GraphGenerator.genGraph(graphSize, 41)

  for (i <- 7 until experiment_times) {
    println(s"Starting hillclimb $i")
    val (graph, fitness, hamiltonian) = Hillclimb(maxEvaluations, startingGraph)
    val json = GraphGenerator.graphToJson(i, graph, fitness, hamiltonian)
    GraphGenerator.writeGraphToFile(s"frontend/resources/hillclimb/$maxEvaluations-evaluations/$graphSize-difficult", i, json)
  }

}
