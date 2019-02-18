package graphs

import scala.collection._

object Vandegriend {

  def funcPruneDegreeTwoNeighbours(edges: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
    val doubleEdges = { for ( i <- edges.keys; if (edges(i).size == 2)) yield (i) }.toSet

    edges.mapValues(n => {
      val doubles = n.filter(a => doubleEdges.contains(a))
      if (doubles.size > 1) doubles.take(2)
      else                  n
    })
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

  // (1, 2, 4, 5)
  // (i, j) -> 
  // def funcPruneTriangleWithTwoDegreePath(edges: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
  //   val doubleEdges = { for ( i <- edges.keys; if (edges(i).size == 2)) yield (i) }
  //   val doubleMap = edges.filter(_._2.size == 2)

  //   def recurse(doubles: Set[Int], paths: List[(Int, Int)] = List()): Set[(Int, Int)] = {
  //     // check if you can extend the head of path
  //     if      (doubles.isEmpty) paths
  //     else if (paths.isEmpty) {
  //       val next           = doubles.head
  //       val nextNeighbours = doubleMap(next)
  //       val first          = (nextNeighbours.head, nextNeighbours.tail.head)

  //       recurse(doubles - next, List(first))
  //     }
  //     else {
  //       val (left, right) = (paths.head_1, paths.head._2)
  //       edges(left)
  //       edges(right)
  //     }
  //       paths.head :: recurse(doubles, paths.tail)
  //     }
  //     else {

  //     }

  //   }
  //   println(recurse(doubleEdges))
  //   edges
  // }

  def pruneTriangleWithTwoDegreePath(edges: Map[Int, mutable.Set[Int]]): 
    mutable.ListBuffer[(Int, Int)] = {
    var changed = false
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
      if (edges(edge1).contains(edge2)) changed = true;
      if (edges(edge1).contains(edge2)) deleted += ((edge1, edge2))
      edges(edge1) -= edge2
      edges(edge2) -= edge1
      doubleVertices -= edge1
      doubleVertices -= edge2
    }

    deleted
  }

  def prune(edges: Map[Int, mutable.Set[Int]]): mutable.ListBuffer[(Int, Int)] = {
    var changed = true
    var deleted = mutable.ListBuffer[(Int, Int)]()
    var newDeleted = mutable.ListBuffer[(Int, Int)]()
  
    while (changed) {
      newDeleted = pruneTriangleWithTwoDegreePath(edges) ++ pruneDegreeTwoNeighbours(edges)
      // newDeleted = pruneDegreeTwoNeighbours(edges)
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

  def check(edges: Map[Int, mutable.Set[Int]]): Boolean = {
    edges.values.forall(_.size >= 2)
  }

  def chooseNextNode(edges: Map[Int, mutable.Set[Int]], 
    stack: mutable.Stack[(mutable.ListMap[Int, Boolean], List[Int], mutable.ListBuffer[(Int, Int)])]): Option[Int] = {

    val degreeMap = edges.mapValues(_.size)
    val sortedOnDegree = if (!stack.isEmpty) { edges(stack.head._2.head)
      .filter(e => !stack.head._1.contains(e) && !stack.head._2.contains(e))
      .toList
      .sortBy(x => degreeMap(x))
    } else degreeMap.toList.sortBy(_._2).map(_._1)

    if (sortedOnDegree.isEmpty) None
    else                        Some(sortedOnDegree.head)
  }

  def solve(graphAsArray: Array[Array[Int]]): (Option[Boolean], Int) = {
    // type Node = (ListMap[Int, Boolean], List[Int], ListBuffer[(Int, Int)])

    val edges   = initEdges(graphAsArray)
    var deleted = prune(edges)

    val stack: mutable.Stack[(mutable.ListMap[Int, Boolean], List[Int], mutable.ListBuffer[(Int, Int)])] = mutable.Stack()
    val start = chooseNextNode(edges, stack).get
    stack.push((new mutable.ListMap[Int, Boolean], List(start), deleted))
    stack.head._1
    var iterations = 0
    while (!stack.isEmpty && !(stack.size == graphAsArray.size)) {
      iterations = iterations + 1 // increment amount of recursions
      deleted    = prune(edges)

      val childNode = chooseNextNode(edges, stack)

      childNode match {
        case None    => { // backtrack, so add deleted back
          stack.head._3.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
          deleted.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
          stack pop
        } 
        case _ if !check(edges) => { // dry
          stack.head._3.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
          deleted.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
          stack pop
        }
        case Some(current) => {
          // this part is incorrect I think.
          // deleted                   += ((i, stack.head._2.head)) // actually delete it maby ?
          // edges(i)                  -= stack.head._2.head
          // edges(stack.head._2.head) -= i

          // delete all other options
          if (stack.head._2.size > 1) {
            val previous       = stack.head._2.head
            val secondPrevious = stack.head._2.tail.head
            edges(previous).filterNot(j => j == current || j == secondPrevious).foreach(j => {
              // println(previous, j, current :: stack.head._2)
              edges(previous) -= j
              edges(j)        -= previous
              deleted         += ((previous, j))
            })
            // println(current, previous, secondPrevious, deleted)
          }
          
          stack.head._1.put(current, true)
          stack.push((new mutable.ListMap[Int, Boolean], current :: stack.head._2, deleted)) }
      }

      // check for last one
      if (stack.size == graphAsArray.size && graphAsArray(stack.head._2.head)(start) == 0) {
        stack.head._3.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })
        stack pop
      }
    }

    if (stack.isEmpty) (Some(false), iterations)
    else               (Some(true),  iterations)
  }
}
