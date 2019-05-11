package graphs

import System.nanoTime
import scala.collection._

object NewVandegriend extends Solver {

  def name = "newvandegriend"

  def funcPruneDegreeTwoNeighbours(edges: Map[Int, mutable.Set[Int]]) = {
    val doubleEdges = { for ( i <- edges.keys; if (edges(i).size == 2)) yield (i) }.toSet


    val del1: Map[Int, Set[Int]] = edges.mapValues(n => {
      val doubles = n.filter(a => doubleEdges.contains(a))
      if (doubles.size > 1) n -- doubles.take(2)
      else                  Set[Int]()
    }).filterNot(_._2.isEmpty)

    val del2  = { for (
      (v1, e) <- edges.toVector;
      v2      <- e;
      if (e.size > 2 && oneConnected(edges.updated(v1, edges(v1) - v2).updated(v2, edges(v2) - v1))) 
    ) yield (v1, v2) }
      .groupBy(_._1)
      .mapValues(e => e.map(_._2).take(2))
      .toVector
      .filter(_._2.size > 1)
      .flatMap(e => {
        (edges(e._1) -- e._2).map(x => (e._1, x))
      })

    val result = mutable.ListBuffer[(Int, Int)]()
    
    for {
      (v1, v2) <- del2
    } {
      edges(v1) -= v2
      edges(v2) -= v1
      result += ((v1, v2))
    }
    
    for {
      (v1, n) <- del1.toVector
      v2      <- n
    } {
      edges(v1) -= v2
      edges(v2) -= v1
      result += ((v1, v2))
    }
    result
  }

  def oneConnected(edges: Map[Int, mutable.Set[Int]]) = {
    var time = 1

    def dfs(
      points: mutable.ListBuffer[Int],
      visited: mutable.Set[Int] = mutable.Set[Int](),
      vertex: Int = 0, 
      visitedTime: mutable.Map[Int, Int] = mutable.Map[Int, Int](),
      parent: mutable.Map[Int, Int] = mutable.Map[Int, Int](),
      lowTime: mutable.Map[Int, Int] = mutable.Map[Int, Int](),
    ): Unit = {

      var childCount = 0
      var isArticulationPoint = false
      visited += vertex
      visitedTime(vertex) = time
      lowTime(vertex) = time
      time = time + 1

      edges(vertex).foreach(adj => {
        if (parent.contains(adj) && parent(adj) == vertex) {
        /* do nothing */ }

        else if (visited.contains(adj)) {
          lowTime(vertex) = scala.math.min(lowTime(vertex), visitedTime(adj))
        }
        else {
          childCount = childCount + 1
          parent += ((adj, vertex))

          dfs(points, visited, adj, visitedTime, parent, lowTime)

          if (visitedTime(vertex) <= lowTime(adj)) {
            isArticulationPoint = true
          }
          else {
            lowTime(vertex) = scala.math.min(lowTime(vertex), lowTime(adj))
          }

        }
      })
    if((!parent.contains(vertex) && childCount >= 2) || parent.contains(vertex) && isArticulationPoint ) {
      points += (vertex);
    }
  }

  val points = mutable.ListBuffer[Int]()
  dfs(points)

  !points.isEmpty
}



  def pruneDegreeTwoNeighbours(edges: Map[Int, mutable.Set[Int]]): 
    mutable.ListBuffer[(Int, Int)] = {
    val doubleEdges    = for { i <- edges.keys; if (edges(i).size == 2) } yield (i)
    val nonDoubleEdges = { for ( i <- edges.keys; if (edges(i).size != 2)) yield (i) } toSet

    val vertexX = { for (
      i <- doubleEdges;
      j <- edges(i)
      if (nonDoubleEdges.contains(j))
    ) yield (j, i) }.groupBy(_._1).filter(_._2.size == 2)
      
    val toDelete = vertexX.map(t => (t._1, edges(t._1).filter(y => nonDoubleEdges.contains(y))))
    val deleted = mutable.ListBuffer[(Int, Int)]()
    for ((i, e) <- toDelete; j <- e) {
      edges(i) -= j
      edges(j) -= i
      deleted += ((i, j))
    }

    deleted
  }

  def pruneTriangleWithTwoDegreePath(edges: Map[Int, mutable.Set[Int]]): 
    mutable.ListBuffer[(Int, Int)] = {
    val doubleEdges = for ( i <- edges.keys; if (edges(i).size == 2)) yield (i)
    var doubleVertices = doubleEdges toSet
    var deleted = mutable.ListBuffer[(Int, Int)]()

    while (!doubleVertices.isEmpty) {
      var edge1 = edges(doubleVertices.head).head
      var edge2 = edges(doubleVertices.head).tail.head
      var path  = mutable.Set(doubleVertices.head)
      doubleVertices -= doubleVertices.head

      while (doubleVertices.contains(edge1) || doubleVertices.contains(edge2)) {
        if (doubleVertices.contains(edge1)) {
          path += edge1 // is this constant ???
          val temp = edges(edge1).filterNot(e => path.contains(e))
          if (temp.size > 0) edge1 = temp.head else return deleted
        }
        if (doubleVertices.contains(edge2)) {
          path += edge2 // same ??
          val temp = edges(edge2).filterNot(e => path.contains(e))
          if (temp.size > 0) edge2 = temp.head else return deleted
        }
      }
      path.foreach(p => doubleVertices -= p)
      if (edges(edge1).contains(edge2)) deleted += ((edge1, edge2))
      edges(edge1) -= edge2
      edges(edge2) -= edge1
      doubleVertices -= edge1
      doubleVertices -= edge2
    }

    deleted
  }

  def prune(edges: Map[Int, mutable.Set[Int]]): mutable.ListBuffer[(Int, Int)] = {
    var deleted = mutable.ListBuffer[(Int, Int)]()
    var newDeleted = mutable.ListBuffer[(Int, Int)]()
  
    var changed = true
    while (changed) {
      newDeleted = pruneTriangleWithTwoDegreePath(edges) ++ funcPruneDegreeTwoNeighbours(edges)
      if (newDeleted.isEmpty) changed = false
      deleted ++= newDeleted
    }
    deleted
  }

  def initEdges(graphAsArray: Array[Array[Int]]): Map[Int, mutable.Set[Int]] = {
    val edges = { for (i <- graphAsArray.indices) yield (i, mutable.Set[Int]()) } toMap

    for {
      i <- graphAsArray.indices
      j <- graphAsArray.indices
      if graphAsArray(i)(j) == 1
    } { 
     edges(i) += j
     edges(j) += i 
    }
    edges
  }

  def disconnectedCheck(edges: Map[Int, mutable.Set[Int]], connectedAll: Set[Int] = Set(1), connectedToCheck: Set[Int] = Set(1)): Boolean = {
    if (connectedAll.size == edges.size) true
    else {
      val n = connectedToCheck.flatMap(e => edges(e)) -- connectedAll

      if (n.isEmpty) false
      else           disconnectedCheck(edges, connectedAll ++ n, n)
    }
  }

  def check(edges: Map[Int, mutable.Set[Int]]): Boolean = {
    edges.values.forall(_.size >= 2) && disconnectedCheck(edges) && !oneConnected(edges)
  }

  def chooseNextNode(edges: Map[Int, mutable.Set[Int]], 
    stack: mutable.Stack[(mutable.ListMap[Int, Boolean], List[Int], mutable.ListBuffer[(Int, Int)])]): Option[Int] = {

    val degreeMap = edges.mapValues(_.size)
    val sortedOnDegree = if (!stack.isEmpty) { edges(stack.head._2.head)
      .filter(e => !stack.head._1.contains(e) && !stack.head._2.contains(e))
      .toList
      .sortBy(x => degreeMap(x))
    } else degreeMap.toList.map(_._1)

    if (sortedOnDegree.isEmpty) None
    else                        Some(sortedOnDegree.head)
  }

  def solve(graphAsArray: Array[Array[Int]], maxTime: Long, cutoff: (Int, Long) => Boolean): 
    (Option[Boolean], Int, Long, Option[List[Int]]) = {
    // type Node = (ListMap[Int, Boolean], List[Int], ListBuffer[(Int, Int)])

    val edges   = initEdges(graphAsArray)

    var deleted = prune(edges)
    
    val stack: mutable.Stack[(mutable.ListMap[Int, Boolean], List[Int], mutable.ListBuffer[(Int, Int)])] = mutable.Stack()
    val start = chooseNextNode(edges, stack).get
    stack.push((new mutable.ListMap[Int, Boolean], List(start), deleted))
    stack.head._1
    var iterations = 0
    val startTime  = nanoTime

    while (!stack.isEmpty && !(stack.size == graphAsArray.size)) {
      if (cutoff(iterations, startTime)) return (None, iterations, nanoTime - startTime, None)
  
      iterations = iterations + 1
      // println(iterations)
      deleted    = prune(edges)
      val childNode = chooseNextNode(edges, stack)

      childNode match {
        case None    => { 
          stack.head._3.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
          deleted.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
          stack pop
        } 
        case _ if !check(edges) => { 
          stack.head._3.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
          deleted.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
          stack pop
        }
        case Some(current) => {
          if (stack.head._2.size > 1) {
            val previous       = stack.head._2.head
            val secondPrevious = stack.head._2.tail.head
            edges(previous).filterNot(j => j == current || j == secondPrevious).foreach(j => {
              edges(previous) -= j
              edges(j)        -= previous
              deleted         += ((previous, j))
            })
          }
          
          stack.head._1.put(current, true)
          stack.push((new mutable.ListMap[Int, Boolean], current :: stack.head._2, deleted)) }
      }

      if (stack.size == graphAsArray.size && graphAsArray(stack.head._2.head)(start) == 0) {
        stack.head._3.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
        stack pop
      }
    }

    if (stack.isEmpty) (Some(false), iterations, nanoTime - startTime, None)
    else               (Some(true),  iterations, nanoTime - startTime, Some(stack.head._2))
  }
}
