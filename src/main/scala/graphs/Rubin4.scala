package graphs

object Rubin4 {

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

  def nextNode(sol: List[Int], checked: Set[Int], out: Map[Int, Set[Int]]): Option[Int] =
    out(sol.head).find(v => !checked.contains(v))

  def isHamiltonian(sol: List[Int]) = 
    (sol.size == graph.size && graph(sol.head)(sol.last) == 1)

  def pruneTwo(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) = {
    val doubleEdges = { 
      for ( i <- edges.keys; if (in(i).size == 2) && out(i) == in(i)) yield (i)
    }.toSet

    // might go something wrong if you dont take exactly the same doubles
    // but that should result in a failure anyway so i think its not a problem
    val newIn = in.mapValues(n => {
      val doubles = n.filter(a => doubleEdges.contains(a))
      if (doubles.size > 1) doubles.take(2)
      else                  n
    })
    val newOut = out.mapValues(n => {
      val doubles = n.filter(a => doubleEdges.contains(a))
      if (doubles.size > 1) doubles.take(2)
      else                  n
    })
    val change = in.exists(n => {
      val doubles = n.filter(a => doubleEdges.contains(a))
      (doubles.size > 1 && n.size > 2)
    })
    (newIn, newOut, change)
  }

  def pruneOne(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) = {
    val inOnes  = in.filter(_._2.size == 1)
    val outOnes = out.filter(_._2.size == 1)
    
    val newIn = for (
      (v1, n) <- in;
    ) yield (v1, n.filter(v => !inOnes.contains(v) || outOnes(v) == v1))
    val newOut = for (
      (v1, n) <- out;
    ) yield (v1, n.filter(v => !inOnes.contains(v) || inOnes(v) == v1))

    (newIn, newOut)
  }

  def prune(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]], sol: List[Int]): (Map[Int, Set[Int]], Map[Int, Set[Int]]) = {
    val (newIn, newOut, change) = pruneTwo(in, out)
    if (change) prune(newIn, newOut, sol)
    else        (newIn, newOut)
  }

  def solve(graph: Array[Array[Int]])] = {

    val in  = createEdgeMap(graph)
    val out = createEdgeMap(graph)

    def recurse(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]], checked: Set[Int] = Set(), sol: List[Int] = List(0)) = {
      val next = nextNode(sol, checked, out)

      if      (isHamiltonian(sol))     Some(true)
      else if (sol.size == graph.size) recurse(in, out, checked + sol.head, sol.tail)
      else {
        next match {
          case None    => Some(false)
          case Some(i) => {
            val (newIn, newOut) = prune(in, out, sol)
            recurse(newIn, newOut, checked, i :: sol) match {
              case None        => None
              case Some(true)  => Some(true)
              case Some(falle) => recurse(in, out, checked + i, sol)
            }
          }
        }
      }
    }
    recurse(in, out)
  }
}
