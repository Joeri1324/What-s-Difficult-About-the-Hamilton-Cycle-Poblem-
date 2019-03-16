package graphs

import spray.json._
import DefaultJsonProtocol._
import java.io.File
import java.io.PrintWriter
import java.nio.file.{Path, Paths, Files}
import scala.util.Try

case class Vertex(id: Int)
case class Endpoint(id: Int)
case class Edges(endpoints: List[Endpoint])
case class Graph(identifier: Int, vertices: List[Vertex], edges: List[Edges], connectivityMap: Map[String, Int], size: Int) {
  def array = {
    val graph = 
      { for {_ <- 0 until size} yield ({ for {_ <- 0 until size} yield (0) } toArray) } toArray

    edges.foreach(e => {
      val v1 = e.endpoints(0).id
      val v2 = e.endpoints(1).id
      graph(v1)(v2) = 1; graph(v2)(v1) = 1
    })
    graph
  }

  def relativeEdgeAmount = {
    edges.size.toFloat / size * 2
  }
}
case class GraphList(graphs: List[Graph])

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val endpointFormat = jsonFormat(Endpoint, "id")
  implicit val edgesFormat = jsonFormat(Edges, "endpoints")
  implicit val vertexFormat = jsonFormat(Vertex, "id")
  implicit val graphFormat = jsonFormat(Graph, "identifier", "vertices", "edges", "connectivityMap", "size")
  implicit object graphListJsonFormat extends RootJsonFormat[GraphList] {
    def read(value: JsValue) = GraphList(value.convertTo[List[Graph]])
    def write(value: GraphList) = JsArray(
      value.graphs.map(graph => JsObject(
        "identifier" -> JsNumber(graph.identifier),
        "vertices"   -> JsArray(
          graph.vertices.map(v => JsObject("id" -> JsNumber(v.id)))
        ),
        "edges" -> JsArray (
          graph.edges.map(e => JsObject("endpoints" -> JsArray(
            JsObject("id" -> JsNumber(e.endpoints(0).id)),
            JsObject("id" -> JsNumber(e.endpoints(1).id)),
          ))
          )
        ),
        "connectivityMap" -> JsObject(graph.connectivityMap.map(c => {
          c._1 -> JsNumber(c._2)
        }))
      ))
    )
  } 
}

import MyJsonProtocol._
import DefaultJsonProtocol._

object GraphReader {

  def graphsFromFile(fileName: String) = 
    scala.io.Source.fromFile(fileName)
      .mkString
      .parseJson
      .convertTo[GraphList]
      .graphs

  def graphsFromFolder(folder: String) = {
    def handleFile(name: String) = 
      scala.io.Source.fromFile(name)
        .mkString
        .parseJson
        .convertTo[Graph]

    for {
      filename <- new File(folder).listFiles
      graph    <- Some(handleFile(filename.toString))
    } yield graph
  }

}

object GraphGenerator { //extends App {

  def genGraph(size: Int, amountOfEdges: Int): Array[Array[Int]] = {

    val r             = scala.util.Random
    val indices       = { for (
      i <- 0 until size; 
      j <- 0 until size;
      if(i > j)) yield (i, j) }.toVector
    val graph =
      {for {i <- 0 until size} yield {for {j <- 0 until size} yield 0} toArray} toArray

    def recursiveGenGraph(currentEdges: Int, indices: Vector[(Int, Int)],
      graph: Array[Array[Int]]): Array[Array[Int]] = {
      require(currentEdges <= amountOfEdges, "Shouldnt be instantiated higher than total amount")
      if (currentEdges == amountOfEdges || indices.isEmpty) graph
      else {
        val index   = r.nextInt(indices.size)
        val newEdge = indices(index)
        recursiveGenGraph(
          currentEdges + 1,
          indices.slice(0, index) ++ indices.slice(index + 1, indices.size),
          graph
            .updated(newEdge._1, graph(newEdge._1).updated(newEdge._2, 1))
            .updated(newEdge._2, graph(newEdge._2).updated(newEdge._1, 1)))
      }
    }

    if (amountOfEdges > 0) recursiveGenGraph(0, indices, graph)
    else                   graph
  }

  def writeGraphToFile(path: String, id: Int, fileContent: String): Unit = {
    def createDir(path: String): File = {
      val dir = new File(path)
      dir.mkdirs()
      dir
    }
    createDir(path)
    val pw = new PrintWriter(new File(s"$path/$id.json"))
    try pw.write(fileContent) finally pw.close()
  }

  def graphToJson(identifier: Int, graph: Array[Array[Int]]): String = {
    val vertices = { for (i <- graph.indices) yield Vertex(i) } toList
    val edges = { for (
      i <- graph.indices;
      j <- graph.indices;
      // i < j to not include duplicatesv
      if graph(i)(j) == 1 && i < j 
    ) yield(Edges(List(Endpoint(i), Endpoint(j)))) } toList
    val degreeMap = { for (
      i <- graph.indices
    ) yield (i.toString, graph(i).count(_ == 1)) } toMap

    Graph(identifier, vertices, edges, degreeMap, graph.size).toJson.prettyPrint
  }

  // require(Try(args(0).toInt).isSuccess, "Input graph size should be int")
  val graphSize      = 10 // args(0).toInt
  val amountOfGraphs = 20
  val maxEdges       = {0 to (graphSize - 1)}.reduce(_ + _)

  for {
    amountOfEdges <- 1 to maxEdges;
    graphNumber   <- 0 to amountOfGraphs;
    graphId       <- Some(((amountOfEdges - 1) * amountOfGraphs) + graphNumber)
  } {
    val json = graphToJson(graphId, genGraph(graphSize, amountOfEdges))
    writeGraphToFile(s"src/main/resources/indexed-$graphSize-node-test-set", graphId, json)
  }

  // val pw = new PrintWriter(new File(s"src/main/resources/indexed-$graphSize-node-test-set.json"))
  // try pw.write(toFile.toJson.prettyPrint) finally pw.close()

  // println(
  //   for (
  //     amountOfEdges <- 1 to maxEdges;
  //     graphNumber   <- 0 to amountOfGraphs;
  //     graphId       <- Some(((amountOfEdges - 1) * amountOfGraphs) + graphNumber)
  //   ) yield (graphId, amountOfEdges)
  // )

    // writeGraphToFile(s"src/main/resources/$graphSize/$avgCon", "graph.json", graph)
}
