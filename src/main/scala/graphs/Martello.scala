package graphs

import scala.collection._

object Martello {

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

  /** Returns the set of edges that must be in any Hamiltonian-Cycle
   *
   */
  def getImplied(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]): Set[(Int, Int)] = {
    val inOne = for (
      (v1, e) <- in.toVector;
      v2      <- e
      if e.size == 1
    ) yield (v2, v1)
    val outOne = for (
      (v1, e) <- out.toVector;
      v2      <- e
      if e.size == 1
    ) yield (v1, v2)

    (inOne ++ outOne).toSet
  }

  def func(oneEdges: Map[Int, Set[Int]], path: List[Int] = List()): 
    (Map[Int, Set[Int]], List[Int]) = {
    if      (oneEdges.isEmpty)              (oneEdges, path)
    else if (path.isEmpty)                  {
      val start = oneEdges.keys.head
      func(oneEdges, List(start))
    }
    else if (!oneEdges.contains(path.head))           (oneEdges, path)
    else if (path.contains(oneEdges(path.head).head)) (oneEdges, path)
    else {
      val newNode = oneEdges(path.head).head
      val newMap  = 
        if (oneEdges(path.head).size == 1) oneEdges - path.head
        else oneEdges.updated(path.head, oneEdges(path.head) - newNode) 
      func(newMap, newNode :: path)
    }
  }

  def recurse(oneEdges: Map[Int, Set[Int]], size: Int, toDelete: List[(Int, Int)] = List()): List[(Int, Int)] = {

    if (oneEdges.isEmpty) toDelete
    else {
      val (newOneEdges, path) = func(oneEdges)
      if (path.size > 2 && path.size < (size - 1)) { recurse(newOneEdges, size, (path.head, path.last) :: toDelete) }
      else               recurse(newOneEdges, size, toDelete)
    }
  }

  def prunePath(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]], implied: Set[(Int, Int)],
    toDelete: List[(Int, Int)] = List()): List[(Int, Int)] = {

    val oneEdges  = implied.groupBy(_._1).mapValues(v => v.map(_._2).toSet)
    recurse(oneEdges, in.size).filter({ case (v1, v2) => out(v1).contains(v2) })
  }

  /** Finds arcs that can be deleted based on the opposites of implied arcs.
    *
    * 
    */
  def pruneEnds(implied: Set[(Int, Int)], incoming: Map[Int, Set[Int]], 
    outgoing: Map[Int, Set[Int]], start: Int) = {
      val emanating = for (
        (v1, v2) <- implied; 
        v3 <- incoming(v2); 
        if !implied.contains(v3, v2) && v2 != start) yield (v3, v2)
      val termanating = for (
        (v1, v2) <- implied;
        v3 <- outgoing(v1);
        if !implied.contains(v1, v3) && v3 != start) yield (v1, v3)
      emanating ++ termanating
  }

  def prune(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]], 
    start: Int, deleted: Set[(Int, Int)] = Set()): Set[(Int, Int)]  = {

    val implied     = getImplied(in, out)
    val pathDeleted = prunePath(in, out, implied)
    val endDeleted  = pruneEnds(implied, in, out, start)
    val delete      = pathDeleted ++ endDeleted
    val nothingChanged = delete.size == 0 
  
    if (nothingChanged) deleted
    else                {
      val (newIn, newOut) = handleDelete(deleted, in, out)
      prune(newIn, newOut, start, deleted ++ delete)
    }
  }

  def handleDelete(toDelete: Set[(Int, Int)], incoming: Map[Int, Set[Int]],
    outgoing: Map[Int, Set[Int]]): (Map[Int, Set[Int]], Map[Int, Set[Int]]) = {
    if (toDelete.isEmpty) (incoming, outgoing)
    else {
      val delete = toDelete.head
      val newIncoming = incoming
        .updated(delete._2, incoming(delete._2) - delete._1)
      val newOutgoing = outgoing
        .updated(delete._1, outgoing(delete._1) - delete._2)

      handleDelete(toDelete.tail, newIncoming, newOutgoing)
    }
  }

def nextNode(sol: List[Int], checked: Set[Int], in: Map[Int, Set[Int]],
      out: Map[Int, Set[Int]]): Option[Int] = {

    val degreeMap = in.keys.map(v => (v, in(v).size + out(v).size)).toMap
    val a = (if (!sol.isEmpty) out(sol.head) else in.keys)
      .toList
      .filter(e => !checked.contains(e) && !sol.contains(e))
      .sortBy(x => degreeMap(x))

    if (a.isEmpty) None
    else           Some(a.head)
  }

  def solve(graph: Array[Array[Int]], maxIter: Int) = {
    // step 0
    val incomingEdges = createEdgeMap(graph)
    val outgoingEdges = createEdgeMap(graph)
    var iterations = 0

    def isHamiltonian(sol: List[Int]) = 
      (sol.size == graph.size && graph(sol.head)(sol.last) == 1)

    def check(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) =
      in.exists(_._2.size == 0) || out.exists(_._2.size == 0)

    def getChildren(i: Int, sol: List[Int], in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) = 
      if (sol.size > 1) (
        ({ for (v <- in(i); if v != sol.head) yield (v, i) } ++ 
        { for (v <- out(sol.head); if v != i) yield (sol.head, v)}).toSet
      )
      else Set()

    def recurseSolve(sol: List[Int], checked: Set[Int] = Set(),
      deleted: Set[(Int, Int)] = Set()): Option[Boolean] = {
      iterations = iterations + 1  

      val (currentIn, currentOut) = handleDelete(deleted, incomingEdges, outgoingEdges)
      val moreDelete              = prune(currentIn, currentOut, sol.last).toSet
      val (in, out)               = handleDelete(moreDelete, currentIn, currentOut)    

      if      (iterations > maxIter)    None
      else if (isHamiltonian(sol))      Some(true)
      else if (!out.contains(sol.head)) Some(false)
      // have to double check
      else if (sol.size == graph.size)  recurseSolve(sol.tail, checked + sol.head, deleted)
      else {
        val child = nextNode(sol, checked, in, out)
        child match {
          case None                      => Some(false)
          case Some(i) if check(in, out) => Some(false)
          case Some(i)                   => {
            val deleteChild = getChildren(i, sol, in, out)

            recurseSolve(i :: sol, deleted = deleted ++ deleteChild ++ moreDelete ) match {
              case None        => None
              case Some(true)  => Some(true)
              case Some(false) => recurseSolve(sol, checked + i, deleted)
            }
          }
        }
      }
    }

    val start = List(nextNode(Nil, Set(), incomingEdges, outgoingEdges).get)
    (recurseSolve(start), iterations)
  }
}
