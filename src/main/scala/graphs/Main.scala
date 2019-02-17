package graphs

import java.io.File
import java.io.PrintWriter
import java.nio.file.{Path, Paths, Files}
import spray.json._

object Main extends App {

  val maxIter = 1000000

  val pw = new PrintWriter("vanhornresults/vandegriend32.csv")
  try {
    GraphReader.graphsFromFolder("src/main/resources/indexed-32-node-test-set").foreach(graph => {
      val (hamiltonian, recursions) = Vandegriend.solve(graph.array)
      println(s"recursions: $recursions hamilton: $hamiltonian")
      val id                 = graph.identifier
      val relativeEdgeAmount = graph.relativeEdgeAmount
      val writeHamiltonian   = hamiltonian match {
        case Some(result) => result.toString
        case None         => "unknown"
      }
      val fileContent        = s"$id,$relativeEdgeAmount,$writeHamiltonian,$recursions\n"

      println(fileContent)
      pw.append(fileContent)
    })
  } finally pw.close()

 // println(Cheeseman.solve(GraphReader.graphsFromFile("src/main/resources/indexed-16-node-test-set.json")(702).array, 100000000))

  // val graph: Array[Array[Int]] = Array(
  //   Array(0, 1, 1, 1),
  //   Array(1, 0, 1, 1),
  //   Array(1, 1, 0, 1),
  //   Array(1, 1, 1, 0),
  // )
  // Rubin2.solve(graph)
  // src/main/resources/16/3.632/0.graph.json
  // println(VanHorn.solve(graphFromFile("src/main/resources/16/2.96/0.graph.json")))
  // println(Martello.solve(graphFromFile("src/main/resources/16/2.96/0.graph.json")))
}
