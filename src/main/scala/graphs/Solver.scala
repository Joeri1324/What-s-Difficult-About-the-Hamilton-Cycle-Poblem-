package graphs

trait Solver {

    def name: String

    def solve(graph: Array[Array[Int]], maxTime: Long, 
        cutoff: (Int, Long) => Boolean): (Option[Boolean], Int, Long, Option[List[Int]])
}

