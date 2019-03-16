package graphs

import scala.collection._

object Rubin {

 /** Transforms graph into a Map for faster lookup
   *
   * Returns a Map that for a vertex: Int gets a list of all neighbour
   * vertices.
   */
  def createEdgeMap(graph: Array[Array[Int]]): Map[Int, Set[Int]] = {
    for {
      i <- graph.indices
    } yield (i -> graph.indices.filter(j => graph(i)(j) == 1).toSet)
  } toMap 
  
  
  //  { for {
  //   i <- graph.indices
  //   j <- graph.indices
  //   if graph(i)(j) == 1
  // } yield (i -> j) 
  // } groupBy (x => x._1) mapValues (x => {x map (x => x._2)} toSet )

  def nextNode(sol: List[Int], checked: Set[Int], in: Map[Int, Set[Int]],
      out: Map[Int, Set[Int]]): Option[Int] = {

    val degreeMap = in.keys.map(v => (v, in(v).size + out(v).size)).toMap
    val a = (if (!sol.isEmpty) out(sol.head) else in.keys)
      .toList
      .filter(e => !checked.contains(e) && !sol.contains(e))
      // .sortBy(x => degreeMap(x))

    if (a.isEmpty) None
    else           Some(a.head)
  }

  def handleDelete(toDelete: Set[(Int, Int)], in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]): (Map[Int, Set[Int]], Map[Int, Set[Int]]) = {
    if (toDelete.isEmpty) (in, out)
    else {
      val delete = toDelete.head
      val newIn  = in
        .updated(delete._2, in(delete._2) - delete._1)
      val newOut = out
        .updated(delete._1, out(delete._1) - delete._2)

      handleDelete(toDelete.tail, newIn, newOut)
    }
  }

  def pruneTwoNeighbour(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) = {
    val unReq = { for (v <- in.keys; if in(v) == out(v) && in(v).size == 2) yield v }.toSet

    for (
      (v1, n) <- in.toVector;
      v2      <- n.filterNot(a => unReq.contains(a))
      if n.filter(a => unReq.contains(a)).size > 1
    ) yield (v2, v1)
  }

  def pruneDirect(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) = {
    val inDel = for (    // directed required arc entering
      (v1, n) <- in.toVector;
      v2      <- n;
      v3      <- out(v2)
      if n.size == 1 && v3 != v1
    ) yield (v2, v3)
    val outDel = for (
      (v1, n) <- out.toVector;
      v2      <- n;
      v3      <- in(v2)
      if n.size == 1 && v3 != v1
    ) yield (v3, v2)

    (inDel ++ outDel).distinct
  }

  def walk(left: Int, right: Int, out: Map[Int, Set[Int]], in: Map[Int, Set[Int]], doubles: Set[Int], path: List[Int]): (Int, Int, List[Int]) = {
    // println("path", path, left, right)
    if      (!doubles.contains(left) && !doubles.contains(right)) (left, right, path)
    else if (doubles.contains(right) && out(right).filter(e => !path.contains(e)).size > 0) {
      val rightOptions = out(right).filter(e => !path.contains(e))  // might be two or one depanding on if the arc is directed
      walk(left, rightOptions.head, out, in, doubles, rightOptions.head :: path )
    }
    else if ( out(left).filter(e => !path.contains(e)).size > 0) {
      val leftOptions = in(left).filter(e => !path.contains(e))  // might be two or one depanding on if the arc is directed
      walk(leftOptions.head, right, out, in, doubles,  leftOptions.head :: path)
    }
    else (left, right, path)
  }

  def recurse(doubles: Set[Int], out: Map[Int, Set[Int]], in: Map[Int, Set[Int]], toDelete: Set[(Int, Int)] = Set()): Set[(Int, Int)] = {
    // println("out", out)
    // println("in", in)
    if (doubles.isEmpty) toDelete
    else {
      val (left, right, path) = walk(doubles.head, doubles.head, out, in, doubles, List(doubles.head))
      if (path.size > 2 && path.size < (out.size - 2)) recurse(doubles -- path.toSet, out, in, toDelete + ((left, right)) + ((right, left)))
      else recurse(doubles -- path.toSet, out, in, toDelete )
    }
  }

  def prunePath(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]],
    toDelete: List[(Int, Int)] = List())  = {

    val trueDouble = { for (v <- in.keys; if in(v) == out(v) && in(v).size == 2) yield v }.toSet
    val doubles = (
      { for (v <- in.keys; if in(v) == out(v) && in(v).size == 2) yield v }.toSet ++
      { for ((v, n) <- in; if n.size == 1) yield v }.toSet ++
      { for ((v, n) <- out; if n.size == 1) yield v }.toSet)

    val useIn = in.map({ case (v, n) =>
      if (trueDouble.contains(v) || n.size == 1) (v, n)
      else                                       (v, Set[Int]())
    })
    val useOut = out.map({ case (v, n) =>
      if   (trueDouble.contains(v) || n.size == 1) (v, n)
      else                                         (v, Set[Int]())
    })
    // List(7, 9, 6, 4, 2, 5, 0, 8, 3, 11, 1, 12, 13, 10, 14, 15)
    // println(doubles)
    // println({ for ((v, n) <- in; if n.size == 1) yield v }.toSet)
    // println({ for ((v, n) <- out; if n.size == 1) yield v }.toSet)
    // println()
    recurse(doubles, useOut, useIn).filter({ case (v1, v2) => out(v1).contains(v2)})
  }

  def pruneOneNeighbour(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) = {
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

  def directionAssignment(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) = {
    // directed assignment doesn't apply since its being incorporated by the directed deletion allready

    // undirected assignment
    val inDel = for {
      v <- in.keys;
      n <- in(v);
      if in(v) == out(v) && in(v).size == 2 && in(n).filter(_ != v).isEmpty
    } yield (n, v)
    val outDel = for {
      v <- in.keys;
      n <- out(v);
      if in(v) == out(v) && in(v).size == 2 && out(n).filter(_ != v).isEmpty
    } yield (v, n)
    inDel ++ outDel
  }

  def prune(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]],
    deleted: Set[(Int, Int)] = Set()): (Map[Int, Set[Int]], Map[Int, Set[Int]], Set[(Int, Int)]) = {
    // have to check if path is long enough

    // have to check if this can be done better by using curried functions
    val delete         = pruneTwoNeighbour(in, out) ++ pruneDirect(in, out) ++ prunePath(in, out) ++ directionAssignment(in, out)
    val nothingChanged = delete.size == 0 
    // println("new prune", nothingChanged)

  
    if (nothingChanged) (in, out, deleted)
    else                {
      val (newIn, newOut) = handleDelete(deleted, in, out)
      prune(newIn, newOut, deleted ++ delete)
    }
  }

  def getChildren(i: Int, sol: List[Int], in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) = 
    if (sol.size > 1) (
      ({ for (v <- in(i); if v != sol.head) yield (v, i) } ++ 
      { for (v <- out(sol.head); if v != i) yield (sol.head, v)}).toSet
    )
    else Set()

  def solve(graph: Array[Array[Int]], maxIter: Int) = {
    // step 0
    val startIn     = createEdgeMap(graph)
    val startOut    = createEdgeMap(graph)
    var iterations = 0

    def isHamiltonian(sol: List[Int]) = 
      (sol.size == graph.size && graph(sol.head)(sol.last) == 1)

    def check(in: Map[Int, Set[Int]], out: Map[Int, Set[Int]]) =
      in.exists(_._2.size == 0) || out.exists(_._2.size == 0)

    def recurseSolve(sol: List[Int], checked: Set[Int] = Set(),
      deleted: Set[(Int, Int)] = Set()): Option[Boolean] = {
      iterations = iterations + 1

      val (curIn, curOut)       = handleDelete(deleted, startIn, startOut)
      val (in, out, moreDelete) = prune(curIn, curOut)

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
            val delChild = getChildren(i, sol, in, out)
            recurseSolve(i :: sol, deleted = deleted ++ moreDelete ++ delChild + ((i, sol.head))) match {
              case None        => None
              case Some(true)  => Some(true)
              case Some(false) => recurseSolve(sol, checked + i, deleted)
            }
          }
        }
      }
    }

    val start = List(nextNode(Nil, Set(), startIn, startOut).get)
    (recurseSolve(start), iterations)
  }
}
