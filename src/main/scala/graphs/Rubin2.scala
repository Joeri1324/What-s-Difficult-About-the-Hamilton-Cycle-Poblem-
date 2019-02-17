package graphs

object Rubin2 {

  type State = (Map[Int, Set[Int]], Map[Int, Set[Int]], Map[Int, Set[Int]])

  // def getVertex(edges: Map[Int, Set[Int]], i: Int, j: Int) = {
  //   val required = 
  // }

  def init(graph: Array[Array[Int]]) = {
    val edges = { for {
      i <- graph.indices
      j <- graph.indices
      if graph(i)(j) == 1
    } yield (i -> j) 
    } groupBy (x => x._1) mapValues (x => {x map (x => x._2)} toSet )

    (Map[Int, Set[Int]](), Map[Int, Set[Int]](), edges)
  }

  def getRequiredDirected(directedIn: Map[Int, Set[Int]], directedOut: Map[Int, Set[Int]]) = {
    val requiredIn  = directedIn.filter(_._2.size == 1).toVector.map(e => (e._1, e._2.head))
    val requiredOut = directedOut.filter(_._2.size == 1).toVector.map(e => (e._2.head, e._1))

    (requiredIn ++ requiredOut).distinct
  }

  def getRequiredUndirected(undirected: Map[Int, Set[Int]]) = {
    for {
      (v1, n) <- undirected.toVector
      v2      <- n
      if n.size == 2
    } yield (v1, v2)
  }

  // for delete undirected it doesn't matter to actually seperate the undirected
  // in a set of required, that can be done on the fly.
  def deleteUndirected(edges: Map[Int, Set[Int]]) = {
    val doubleEdges = { for ( i <- edges.keys; if (edges(i).size == 2)) yield (i) }.toSet

    val r = for {
      (v1, n) <- edges.toVector
      doubles <- Some(n.filter(a => doubleEdges.contains(a)))
      if doubles.size >= 2
    } yield (v1, n -- doubles)
    for (
      (v1, n) <- r;
      v2 <- n
    ) yield (v1, v2)
  }

  // lets see if directed can be done on the fly as well.
  def deleteDirected(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) = {
    val inDel = for (
      (v1, n) <- in.toVector;
      v2      <- n;
      other   <- out(v2);
      if (n.size == 1) && other != v1
    ) yield (v2, other)

    val outDel = for (
      (v1, n) <- out.toVector;
      v2      <- n;
      other   <- in(v2);
      if (n.size == 1) && other != v1
    ) yield (other, v2)
    
    (inDel ++ outDel).distinct
  }

  def getRequired(directedIn: Map[Int, Set[Int]], directedOut: Map[Int, Set[Int]],
    undirected: Map[Int, Set[Int]]) = {

    val requiredDirected   = getRequiredDirected(directedIn, directedOut)
    val requiredUndirected = getRequiredUndirected(undirected)
  }

  def chooseNextNode(sol: List[Int], checked: Set[Int], in: Map[Int, Set[Int]],
    out: Map[Int, Set[Int]], un: Map[Int, Set[Int]]): Option[Int] = {

    val current = sol.head
    if (sol.isEmpty) Some(0)
    else             (un(current) ++ out(current)).find(
      e => !sol.contains(e) && !checked.contains(e))
  }


  def solve(graph: Array[Array[Int]], maxIter: Int) = {
    val state = init(graph)
    // getRequired(state._1, state._2, state._3)

    var iterations = 0

    def isHamiltonian(sol: List[Int]) = 
      (sol.size == graph.size && graph(sol.head)(sol.last) == 1)

    def recurseSolve(sol: List[Int], checked: Set[Int] = Set()): Option[Boolean] = {

      iterations = iterations + 1

      if      (iterations > maxIter)      None
      else if (isHamiltonian(sol))        Some(true)
      else if (!state._2.contains(sol.head) &&
        !state._3.contains(sol.head))     Some(false)
      else if (sol.size == graph.size)    recurseSolve(sol.tail, checked + sol.head)
      else {
        // there is a lookup in sol which is a O(n) operation on each branch
        val child = chooseNextNode(sol, checked, state._1, state._2, state._3)
      
        // Option is used to allow for break of at maximum iterations
        child match {
          case None    => Some(false)
          case Some(i) => 
            recurseSolve(i :: sol) match {
              case None        => None
              case Some(true)  => Some(true)
              case Some(false) => recurseSolve(sol, checked + i)
            }
        }
      }
    }
  }
}
zw