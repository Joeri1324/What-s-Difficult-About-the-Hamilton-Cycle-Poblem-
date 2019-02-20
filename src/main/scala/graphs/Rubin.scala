package graphs

import scala.collection._

case class Edge(val i: Int, val j: Int) {
  var direction = 0
  var set       = 0

  def directionUnknown = direction == 0
  def getSet = set
  def setRequired = { 
    if (set != 1) { set = 1; true }
    else false
  }
  def isRequired = set == 1
  def isUndecided = set == 0
  def isDeleted = set == 2
  def setDeleted = { 
    if (set != 2) { set = 2; true }
    else false
  }
  def isForward = direction == 1
  def isBackward = direction == 2
  def setForward = { direction = 1 }
  def setBackward = { direction = 2 }
  def isEntering(x: Int) = {
    if ((x == j && isForward) || (x == i && isBackward)) true
    else false
  }
  def setEntering(x: Int) = {
    if      (x == j && direction != 1) { direction = 1; true }
    else if (direction != 2)           { direction = 2; true }
    else                                                false
  }
  def isLeaving(x: Int) = {
    if ((x == i && isForward) || (x == j && isBackward)) true
    else false
  }
  def setLeaving(x: Int) = {
    if      (x == j && direction != 2) { direction = 2; true }
    else if (direction != 1)           { direction = 1; true }
    else                                                false             
  }

  override def toString = s"($i, $j) $direction $set" 
}

object Rubin {


  // data structure?
  // edge and map to edge?
  // type Edge = (Int, Int, Int, Int) // (Vertex, Vertex, Direction, Set)
  // direction [0, 1, 2] = [Undecided, Forward, Backward]
  // set       [0, 1, 2] = [Undecided, Required, Deleted]

  def init(graph: Array[Array[Int]]) = {
    val edges = { for (i <- graph.indices) yield (i, mutable.Set[Edge]()) } toMap

    for {
      i <- graph.indices
      j <- graph.indices
      if graph(i)(j) == 1 && i > j
    } { 
      val edge = new Edge(i, j)
      edges(i) += edge
      edges(j) += edge
    }
    edges
  }

  def required(edgemap: Map[Int, Set[Edge]]) = {
    var changed = false
    for (v <- edgemap.keys) {
      val edges           = edgemap(v).filterNot(_.isDeleted)
      val directedEdges   = edges.filterNot(_.directionUnknown)
      val undirectedEdges = edges.filter(_.directionUnknown)

      // rule 1, i think it doesnt apply when there is an undirected edge still
      if (undirectedEdges.isEmpty) {
        // if (v == 9 || v == 11) {
        //   println("chicken1", edges)
        // }
        val forward = directedEdges.filter(_.isLeaving(v))
        val backward = directedEdges.filter(_.isEntering(v))
        if (forward.size == 1) { changed = changed || forward.head.setRequired }
        if (backward.size == 1) { changed = changed || backward.head.setRequired }
      }
      
      // rule 2
      if (edges.size == 2) {
        // if (v == 9 || v == 11) {
        //   println("chicken2", edges, edgemap(v).filter(_.isDeleted))
        // }
        edges.foreach(_.setRequired)
      }

      
    }
    changed
  }

  def direction(edgemap: Map[Int, Set[Edge]]) = {
    var changed = false
    for (v <- edgemap.keys) {
      val edges = edgemap(v).filterNot(_.isDeleted)
      val directedEdges   = edges.filterNot(_.directionUnknown)
      val undirectedEdges = edges.filter(_.directionUnknown)

      // rule 1
      val requiredDirected = directedEdges.filter(_.isRequired)
      if (requiredDirected.size == 1 && requiredDirected.head.isEntering(v)) {
        undirectedEdges.foreach({ e => changed = changed || e.setLeaving(v) })
      }
      if (requiredDirected.size == 1 && requiredDirected.head.isLeaving(v)) {
        undirectedEdges.foreach(e => { changed = changed || e.setEntering(v) })
      }
      // rule 2
      val undirectedRequired = undirectedEdges.filter(_.isRequired)
      if (undirectedRequired.size == 1 && undirectedEdges.size == 1) {
        if (directedEdges.forall(_.isEntering(v))) {
          changed = changed || undirectedRequired.head.setLeaving(v)
        }
        if (directedEdges.forall(_.isEntering(v))) {
          changed = changed || undirectedRequired.head.setLeaving(v)
        }
      }
    }
    changed
  } // (11, 2)

  def deleteCycle(edgemap: Map[Int, Set[Edge]]) = {
    var changed = false
    val requiredEdgemap = edgemap.mapValues(edges => edges.filter(_.isRequired))
    val requiredEdges   = requiredEdgemap.values.flatMap(_.toList).toSet
    for (edge1 <- requiredEdges) {
      var path = mutable.MutableList[Edge](edge1)
      for (edge2 <- requiredEdges; if edge2 != edge1) {
        if (path.head.isForward && path.head.j == edge2.i && edge2.isLeaving(edge2.i)) {
          edge2 +=: path
        }
        if (path.head.isBackward && path.head.i == edge2.j && edge2.isLeaving(edge2.j)) {
          edge2 +=: path
        }
        if (path.head.directionUnknown && edge2.directionUnknown && 
           (path.head.i == edge2.j || path.head.j == edge2.i ||
            path.head.i == edge2.i || path.head.j == edge2.j)) {
           edge2 +=: path
        }
      }
      if (path.size > 1) {
        val first = 
          if (path.tail.head.i != path.head.i && path.head.i != path.tail.head.j) path.head.i
          else path.head.j
        val second = 
          if (path.last.i != path(path.size - 2).i && path.last.i != path(path.size - 2).j) path.last.i
          else path.last.j
        for (
          edge <- edgemap(first).find(e => (e.i == first && e.j == second) || (e.j == first && e.i == second))
        ) {
          val del = edge.setDeleted
          changed = changed || del
        }
      }
    }

    changed
  }

  def delete(edgemap: Map[Int, Set[Edge]]) = {
    // } // List(10, 7, 2, 11, 5, 9, 4, 3, 6, 8, 1, 0)
    var changed = false
    for (v <- edgemap.keys) {
      val edges    = edgemap(v).filterNot(_.isDeleted)
      val required = edges.filter(_.isRequired)
      // why (11, 2) deleted ?
      // rule 1
      if (required.size == 2) {
        val undecided = edges.filter(_.isUndecided)
        undecided.foreach(e => {
          changed = changed || e.setDeleted}
        )
      }
      // rule 2
      val directedRequired = required.filterNot(_.directionUnknown)
      if (directedRequired.size == 1) {
        val directedUndecided = edges.filter(_.isUndecided)
        if (directedRequired.head.isForward) {
          directedUndecided.foreach(e => if (e.isForward) {
            changed = changed || e.setDeleted
          })
        }
        if (directedRequired.head.isBackward) {
          directedUndecided.foreach(e => if (e.isBackward) {
            changed = changed || e.setDeleted
          })
        }
      }
    }
    var changedLoop = deleteCycle(edgemap)
    changedLoop || changed
  }

  def fzes(edges: Map[Int, Set[Edge]]): Boolean = {
    val requiredEdgemap = edges.mapValues(edges => edges.filter(_.isRequired))
    val requiredEdges   = requiredEdgemap.values.flatMap(_.toList).toSet
    for (edge1 <- requiredEdges) {
      var path = mutable.MutableList[Edge](edge1)
      for (edge2 <- requiredEdges) {
        if (edge2 != path.head && 
            (edge2.i == path.head.i || edge2.i == path.head.j ||
             edge2.j == path.head.i || edge2.j == path.head.j)) {
               path += edge2
             }
      }
      if (path.size > 1 && (path.distinct.size < path.size)) return true
    }
    false
  }

  def failure(edges: Map[Int, Set[Edge]]): Boolean = {
    // 1, 2
    val notDeleted = edges.mapValues(e => e.filterNot(_.isDeleted))
    val required   = edges.mapValues(e => e.filter(_.isRequired))
    val f1f2 = notDeleted.values.forall(_.size > 1)
    val f3 = notDeleted.keys.forall(v => ! {
      val edgeS = notDeleted(v)
      (edgeS.forall(e => !e.directionUnknown && e.isEntering(v)) ||
       edgeS.forall(e => !e.directionUnknown && e.isLeaving(v)))
    })
    val f4f5 = required.keys.forall(v => ! {
      ! (required(v).size == 3) && // f5
      ! (required(v).size == 2 &&
        ( (required(v).head.isEntering(v) && required(v).tail.head.isEntering(v)) ||
          (required(v).head.isLeaving(v) && required(v).tail.head.isLeaving(v))))
    })
    val f6 = fzes(edges)
    f1f2 || f3 || f4f5 || f6
  }

  def setPathRequired(edgemap: Map[Int, Set[Edge]], path: List[Int]) = {
    for (x <- path.sliding(2)) {
      for (
        edge <- edgemap(x(0)).find(e => e.j == x(1) || e.i == x(1))
      ) {
        edge.setLeaving(x(1))
        edge.setRequired
      }
    }
  }

  def admissable(edges: Map[Int, Set[Edge]], path: List[Int]) = {
    setPathRequired(edges, path)
    var changed = true
    while (changed) {
      var changed1 = required(edges)
      var changed2 = direction(edges)
      var changed3 = delete(edges)

      changed = changed1 || changed2 || changed3
    }
    failure(edges)
  }

  def solve(graph: Array[Array[Int]]): (Option[Boolean], Int) = {
    type Node = (mutable.ListMap[Int, Boolean], List[Int]) 

    val edgess: Map[Int, List[Int]] = { for {
      i <- graph.indices
      j <- graph.indices
      if graph(i)(j) == 1
    } yield (i -> j) } groupBy (x => x._1) mapValues (x => {x map (x => x._2)} toList )

    var edges = init(graph)

    val start = 0
    val stack = mutable.Stack[Node]()
    stack.push((new mutable.ListMap[Int, Boolean], List(start)))
    val path = mutable.Stack[Int](start)
    var iterations = 0

    while (!stack.isEmpty && !(stack.size == graph.size)) {
      edges = init(graph)
      iterations = iterations + 1 // increment amount of recursions
      val childNode = edgess(stack.head._2.head).find(e => !stack.head._1.contains(e) && !stack.head._2.contains(e))
      childNode match {
        case None    => { stack pop }
        case Some(i) if admissable(edges, i :: stack.head._2) => { 
          stack.head._1.put(i, true)
          stack.push((new mutable.ListMap[Int, Boolean], i :: stack.head._2)) }

        case _  => { stack pop }
      }

      // check for last one
      if (stack.size == graph.size && graph(stack.head._2.head)(start) == 0) {
        stack pop
      }
    } // List(10, 7, 2, 11, 5, 9, 4, 3, 6, 8, 1, 0)
    // println(edgess)

    if (stack.isEmpty) (Option(false), iterations)
    else               {
      println( stack.head._2 )
      (Option(true), iterations)
    }
  }
}
