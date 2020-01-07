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
    val folder  = s"src/main/resources/indexed-$size-node-test-set.json"
  
    try {
      pw.append("[\n")
      
      GraphReader.graphsFromFile(folder).foreach(graph => {
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
      pw.append("]\n")
    } finally pw.close()
  }

  // def mutations(algorithm: Solver, id: Int, size: Int, mutations: Int) = {
  //   val graph = GraphReader.graphsFromFolder(s"src/main/resources/indexed-$size-node-test-set")
  //     .filter(_.identifier == id)(0)
  //   val name = algorithm.name
  //   var first = true
  //   val pw = new PrintWriter(s"results/mutations/random-$name-$size.json")
  //   val (newGraph, removed, added) = graph.randomMutation

  //   try {
  //     pw.append("[\n")
  //     for (
  //       _ <- 0 until mutations
  //     ) {
  //       if (!first) { pw.append(",")}
  //       first = false 
  //       val (newGraph, removed, added) = graph.randomMutation
  //       val (hamiltonian, recursions, time, path) =
  //         algorithm.solve(newGraph.array, cutoff(10000.toLong * 10000.toLong, 100000, "iterations"))

  //       val writeHamiltonian = hamiltonian match {
  //         case Some(result) => result.toString
  //         case None         => "null"
  //       }
  //       val removedd = (removed.endpoints(0).id, removed.endpoints(1).id)

  //       pw.append(
  //         s"""{
  //           "removed": "$removedd", "added": "$added", "iterations": $recursions,
  //           "hamiltonian": $writeHamiltonian}\n"""
  //       )
  //       println(recursions)
  //   }

  //     pw.append("]")
  //   } finally pw.close()
  // }

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

  // mutations(Vandegriend, 1683, 32, 100)

  // hillClimb(Vandegriend, 8, 1000, 20)

  // hillExperiment(Rubin, 16, 1000)

  // val algos = List( NeighbourPruning, PathPruning, NeighbourAndPathPruning)
  val algos = List(CheckDisconnected, CheckOneDegree, CheckOneConnected)
  //algos.foreach(a =>  hillExperiment(a, 16, 10000))
  // algos.foreach(a =>  hillExperiment(a, 24, 1000))
  //algos.foreach(a =>  hillExperiment(a, 32, 10000))
  // val algos = List(Sleegers)
  algos.foreach(a => experiment(a, 16, "iterations"))
  algos.foreach(a => experiment(a, 16, "time"))
  // algos.foreach(a => experiment(a, 24, "iterations"))

  // println(Sleegers.solve(GraphReader.graphsFromFolder("src/main/resources/indexed-32-node-test-set")
  //       .filter(x => x.identifier == 1684)(0).array, 10000.toLong * 10000.toLong, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))

}

