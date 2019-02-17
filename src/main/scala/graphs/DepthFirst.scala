package graphs

trait DepthFirst {

  def chooseNextNode(sol: List[Int], edges: Map[Int, Set[Int]], checked: Set[Int]): Option[Int] 

  def check(edges: Map[Int, Set[Int]]): Boolean

 /** Transforms graph into a Map for faster lookup
   *
   * Returns a Map that for a vertex: Int gets a list of all neighbour
   * vertices.
   */
  def createEdgeMap(graph: Array[Array[Int]]): Map[Int, Set[Int]] = { for {
    i <- graph.indices
    j <- graph.indices
    if graph(i)(j) == 1
  } yield (i -> j) 
  } groupBy (x => x._1) mapValues (x => {x map (x => x._2)} toSet )

 /** Recursive depth-first search solution Hamilton-Cycle  
   * 
   * Checks on each iteration if a child node can be found, if it can
   * that branch is further explored recursively. If there is no solution
   * in a branch it will return false on the left hand side of the or 
   * operator. The right hand side of the operator is a recursive call
   * where this time it doesn't check for that child.
   */
  def solve(graph: Array[Array[Int]], maxIter: Int): (Option[Boolean], Int) = {
    val edges      = createEdgeMap(graph)
    var iterations = 0

    def isHamiltonian(sol: List[Int]) = 
      (sol.size == graph.size && graph(sol.head)(sol.last) == 1)

    def recurseSolve(sol: List[Int], checked: Set[Int] = Set()): Option[Boolean] = {

      iterations = iterations + 1

      if      (iterations > maxIter)      None
      else if (isHamiltonian(sol))        Some(true)
      else if (!edges.contains(sol.head)) Some(false)
      else if (sol.size == graph.size)    recurseSolve(sol.tail, checked + sol.head)
      else {
        // there is a lookup in sol which is a O(n) operation on each branch
        val child = chooseNextNode(sol, edges, checked)
      
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

    if (check(edges)) (recurseSolve(List(chooseNextNode(Nil, edges, Set()).get)), iterations)
    else              (Some(false), 1)
  }
}

object Naked extends DepthFirst {

  /** Naked depth-first search choses the first node.
    */
  def chooseNextNode(sol: List[Int], edges: Map[Int, Set[Int]], checked: Set[Int]) = 
    if (sol.isEmpty) Some(0)
    else             edges(sol.head).find(e => !sol.contains(e) && !checked.contains(e))

  def check(edges: Map[Int, Set[Int]]) = true
}

object Vanhorn extends DepthFirst {

   /** Van Horn's algorithm chooses the node which has the lowest degree.
    */
  def chooseNextNode(sol: List[Int], edges: Map[Int, Set[Int]], checked: Set[Int]): Option[Int] = {
    val degreeMap = edges.mapValues(_.size)
    val sorted    = if (sol.isEmpty) edges.keys else edges(sol.head)
      .filter(e => !sol.contains(e) && !checked.contains(e))
      .toList
      .sortBy(x => degreeMap(x))

    if (sorted.isEmpty) None
    else                Some(sorted.head)
  }

  def check(edges: Map[Int, Set[Int]]) = true
}

object VanhornCheck extends DepthFirst {

   /** Van Horn's algorithm chooses the node which has the lowest degree.
    */
  def chooseNextNode(sol: List[Int], edges: Map[Int, Set[Int]], checked: Set[Int]): Option[Int] = {
    val degreeMap = edges.mapValues(_.size)
    val sorted    = if (sol.isEmpty) edges.keys else edges(sol.head)
      .filter(e => !sol.contains(e) && !checked.contains(e))
      .toList
      .sortBy(x => degreeMap(x))

    if (sorted.isEmpty) None
    else                Some(sorted.head)
  }

  /** Employ initial check for disconnected graph.
    */
  def check(edges: Map[Int, Set[Int]]) = {
    val degreeMap        = edges.mapValues(_.size)
    val graphIsConnected = edges.forall(_._2.size >= 2)
    val neighbour        = edges.forall(_._2.filter(n => degreeMap(n) == 2).size <= 2)

    graphIsConnected && neighbour
  }
}

object Cheeseman extends DepthFirst {

   /** Cheeseman's algorithm chooses the node which has the highest degree.
    */
  def chooseNextNode(sol: List[Int], edges: Map[Int, Set[Int]], checked: Set[Int]): Option[Int] = {
    val degreeMap = edges.mapValues(_.size)
    val sorted    = if (sol.isEmpty) edges.keys else edges(sol.head)
      .filter(e => !sol.contains(e) && !checked.contains(e))
      .toList
      .sortBy(x => degreeMap(x))
      .reverse

    if (sorted.isEmpty) None
    else                Some(sorted.head)
  }

  def check(edges: Map[Int, Set[Int]]) = true
}
