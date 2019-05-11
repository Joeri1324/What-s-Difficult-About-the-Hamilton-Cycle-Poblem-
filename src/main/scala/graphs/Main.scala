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
    val folder  = s"src/main/resources/indexed-$size-node-test-set"
    try {
      pw.append("[\n")
      
      GraphReader.graphsFromFolder(folder).foreach(graph => {
        if (!first) { pw.append(",")}
        first = false 
        val (hamiltonian, recursions, time, path) = algorithm
          .solve(graph.array, maxTime, cutoff(maxTime, maxIter, typ))

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
          s""" "hamiltonian": $writeHamiltonian, "iterations": $recursions,""" +
          s""" "nanoseconds": $time\n, "algorithm": "$name", "path": $pathString}""" }

        pw.append(fileContent)
      })
      pw.append("]\n")
    } finally pw.close()
  }

  // val algos = List(Martello, Vandegriend, Naked, Cheeseman, Vanhorn)
  val algos = List(Sleegers)
  algos.foreach(a => experiment(a, 32, "time"))
  algos.foreach(a => experiment(a, 32, "iterations"))

  // println(Sleegers.solve(GraphReader.graphsFromFolder("src/main/resources/indexed-32-node-test-set")
  //       .filter(x => x.identifier == 1684)(0).array, 10000.toLong * 10000.toLong, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))

}

