package graphs

import java.io.File
import java.io.PrintWriter
import java.nio.file.{Path, Paths, Files}
import spray.json._
import java.lang.management._
import System._



object Main extends App {

  /** Perform experiment for a certain algorithm
   *  
   *  Iterates through the folder that contains graphs of size [size]
   *  solves each iteration in the folder with [algorithm] and records
   *  the amount of iterations and the wall clock time.
   */

  def cutoff(maxTime: Long, maxIter: Int, timeOrIterations: String = "time")
    (curIter: Int, startTime: Long): Boolean =
    if (timeOrIterations == "time") nanoTime - startTime > maxTime
    else                            curIter > maxIter

  def experiment(algorithm: Solver, size: Int, typ: String) = {

    val maxTime = 10000.toLong * 10000.toLong
    val maxIter = 100000
    val name    = algorithm.name 
    var first   = true
    val pw      = new PrintWriter(s"results/$typ/result-$name-$size.json")
  
    try {
      pw.append("[\n")

      if (size == 32) {
        GraphReader.graphsFromFolder("src/main/resources/indexed-32-node-test-set").foreach(graph => {
             if (!first) { pw.append(",")}
        first = false 
        val (hamiltonian, recursions, time, path) = algorithm
          .solve(graph.array, cutoff(maxTime, maxIter, typ))

        println(s"$name recursions: $recursions hamilton: $hamiltonian")
        val id                 = graph.identifier
        val relativeEdgeAmount = graph.relativeEdgeAmount
        val pathString = path match {
          case Some(p) => { "[" + p.map(v => s"""{"id": $v}""")
            .toString
            .stripPrefix("List(")
            .stripSuffix(")").trim + "]" }
          case None    => "null"
        }
        val writeHamiltonian = hamiltonian match {
          case Some(result) => result.toString
          case None         => "null"
        }
        val fileContent = { s"""{"id": $id, "degree": $relativeEdgeAmount, """ +
          s""" "hamiltonian": $writeHamiltonian, "iterations": $recursions, "size": $size, """ +
          s""" "nanoseconds": $time\n, "algorithm": "$name", "path": $pathString}""" }

        pw.append(fileContent)
        })
      } else {
        GraphReader.graphsFromFile(s"src/main/resources/indexed-$size-node-test-set.json").foreach(graph => {
             if (!first) { pw.append(",")}
        first = false 
        val (hamiltonian, recursions, time, path) = algorithm
          .solve(graph.array, cutoff(maxTime, maxIter, typ))

        println(s"$name recursions: $recursions hamilton: $hamiltonian")
        val id                 = graph.identifier
        val relativeEdgeAmount = graph.relativeEdgeAmount
        val pathString = path match {
          case Some(p) => { "[" + p.map(v => s"""{"id": $v}""")
            .toString
            .stripPrefix("List(")
            .stripSuffix(")").trim + "]" }
          case None    => "null"
        }
        val writeHamiltonian = hamiltonian match {
          case Some(result) => result.toString
          case None         => "null"
        }
        val fileContent = { s"""{"id": $id, "degree": $relativeEdgeAmount, """ +
          s""" "hamiltonian": $writeHamiltonian, "iterations": $recursions, "size": $size, """ +
          s""" "nanoseconds": $time\n, "algorithm": "$name", "path": $pathString}""" }

        pw.append(fileContent)
        })
      }
      // read.foreach(graph => {
      // })
      pw.append("]\n")
    } finally pw.close()
  }

  def randomMutation(graph: Array[Array[Int]]) = {
    val newGraph = graph.clone()
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

    newGraph(deleteEdge._1)(deleteEdge._2) = 0
    newGraph(deleteEdge._2)(deleteEdge._1) = 0
    newGraph(newEdge._1)(newEdge._2) = 1
    newGraph(newEdge._2)(newEdge._1) = 1

    newGraph
  }

  def hillClimb(
    algorithm:     Solver,
    size:          Int,
    mutations:     Int,
    amountOfEdges: Int,
    pw:            PrintWriter,
    firstHill:     Boolean
  ) = {
    
    var graph    = GraphGenerator.genGraph(size, amountOfEdges)
    var changed  = true
    var maxEdges = IndexedSeq[(Int, Int)]()
    val name = algorithm.name
    var (maxHamilton, maxRecursions, _, paths) = algorithm.solve(graph, cutoff(10000.toLong * 10000.toLong, 100000, "iterations"))
    val writeHamiltonian = maxHamilton match {
      case Some(result) => result.toString
      case None         => "null"
    }
    val pathString = paths match {
      case Some(p) => { "[" + p.map(v => s"""{"id": $v}""")
        .toString
        .stripPrefix("List(")
        .stripSuffix(")").trim + "]" }
      case None    => "null"
    }
    val degree = amountOfEdges.toFloat / size * 2

    if (!firstHill) { pw.append(",") }
    pw.append(s"""{ "iterations": $maxRecursions, "degree": $degree, 
                    "hamiltonian": $writeHamiltonian, "algorithm": "$name", 
                    "path": $pathString }  \n""")



    while(changed) {
      var i        = 0
      var improved = false
      changed = false
      while(i < mutations && !improved) {
        val mutatedGraph = randomMutation(graph)
        val (hamiltonian, recursions, time, path) =
          algorithm.solve(mutatedGraph, cutoff(10000.toLong * 10000.toLong, 100000, "iterations"))
        if (recursions > maxRecursions) {
          changed       = true
          graph         = mutatedGraph
          maxRecursions = recursions
          improved      = true
          val writeHamiltonian = hamiltonian match {
            case Some(result) => result.toString
            case None         => "null"
          }
          val pathString = path match {
            case Some(p) => { "[" + p.map(v => s"""{"id": $v}""")
              .toString
              .stripPrefix("List(")
              .stripSuffix(")").trim + "]" }
            case None    => "null"
          }
          val degree = amountOfEdges.toFloat / size * 2
          pw.append(",")
          pw.append(s"""{ "iterations": $maxRecursions, "degree": $degree, 
                          "hamiltonian": $writeHamiltonian, "algorithm": "$name", 
                          "path": $pathString }  \n""")
        }
        
        i = i + 1
      }
      maxEdges = for (
        i <- graph.indices;
        j <- graph.indices;
        if graph(i)(j) == 1 && i < j 
      ) yield (i, j)

    }
    println(algorithm.name, maxRecursions)
    (maxRecursions, maxEdges)
  }

  def hillExperiment(algorithm: Solver, size: Int, mutations: Int) = {
    val maxEdges = {0 to (size - 1)}.reduce(_ + _)
    val name = algorithm.name
    val pw = new PrintWriter(s"results/hillclimb/$name-$size.json")
    var firstHill = true

    // hillClimb(algorithm, size, mutations, maxEdges - 1)
    try {
      pw.append("[\n")
      for {
        i <- 1 until maxEdges
      } {
        val (recursions, edges) = hillClimb(algorithm, size, mutations, i, pw, firstHill)
        firstHill = false

      }
      pw.append("]")
    } finally pw.close()
  }

  val algos = List(
    //ArbitraryHeuristic,
    // LowHeuristic,
    //HighHeuristic,
    // PathPruning,
    // NeighbourPruning,
    // NeighbourAndPathPruning,
    // CheckOneConnectedWithPruning,
    // CheckDisconnectedWithPruning,
    // CheckOneDegreeWithPruning,
    //CheckAllWithPruning,
    // CheckDisconnected,
    //CheckOneDegree,
    //CheckOneConnected
    // Martello,
    // Vandegriend,
    Rubin
  )

  // algos.foreach(a => experiment(a, 16, "iterations"))
  // algos.foreach(a => experiment(a, 16, "time"))
  // algos.foreach(a => experiment(a, 24, "iterations"))
  // algos.foreach(a => experiment(a, 24, "time"))
  algos.foreach(a => 
    experiment(a, 32, "iterations"))
  algos.foreach(a =>
   experiment(a, 32, "time"))

}
