package graphs

import java.io.File
import java.io.PrintWriter
import java.nio.file.{Path, Paths, Files}
import spray.json._
import java.lang.management._
import System._

object Main extends App {

  val maxTime = 100000.toLong * 100000.toLong
  val pw = new PrintWriter("results/vanhorn16.csv")

  def getTime = {
    val thread = ManagementFactory.getThreadMXBean()
    thread.getAllThreadIds.toVector.map(id => thread.getThreadCpuTime(id)).reduce(_ + _)
  }

  try {
    GraphReader.graphsFromFile("src/main/resources/indexed-16-node-test-set.json").foreach(graph => {
      val (hamiltonian, recursions, time) = Vanhorn.solve(graph.array, maxTime)

      println(s"recursions: $recursions hamilton: $hamiltonian")
      val id                 = graph.identifier
      val relativeEdgeAmount = graph.relativeEdgeAmount
      val writeHamiltonian   = hamiltonian match {
        case Some(result) => result.toString
        case None         => "unknown"
      }
      val fileContent        = s"$id,$relativeEdgeAmount,$writeHamiltonian,$recursions,$time\n"

      println(fileContent)
      pw.append(fileContent)
    })
  }  finally pw.close()

  // println(getTime)

  // println(
  //   Vanhorn.solve(
  //   GraphReader.graphsFromFolder(
  //     "src/main/resources/indexed-32-node-test-set")(100).array, maxTime))

  // println(getTime)
}
