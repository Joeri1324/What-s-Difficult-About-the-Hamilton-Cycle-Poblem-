// package graphs

// object Most extends App {

//     def allEdges(n: Int) = (n * (n - 1)) / 2

//     def combinations(n: Int): Stream[List[Int]] = {
//         if (n == 0) Stream(List())
//         else combinations(n - 1).flatMap(s => Stream(1 :: s, 0 :: s))
//     }

//     def toArray(n: Int, combo: List[Int]) = {
//        val graph = { for {_ <- 0 until n} yield ({ for {_ <- 0 until n} yield (0) } toArray) } toArray

//         var index = 0
//         for {
//             i <- 0 to n - 1
//             j <- 0 until i
//         } {
//             graph(i)(j) = combo(index)
//             graph(j)(i) = combo(index)
//             index = index + 1
//         }
//         graph
//     }

//     var max: Array[Array[Int]] = Array(Array[Int]())
//     var value: Int = 0
//     val size = 8
//     var paths = List[Int]()

//     def printArray(graph: Array[Array[Int]]) = {
//         for {
//             line <- graph
//         } {
//             println(line.toVector)
//         }
//     }

//     var count = 0
//     for (
//         instance <- combinations(allEdges(size))
//     ) {
//         count = count + 1
//         val graph = toArray(size, instance)
//         val (hamiltonian, recursions, time, path) =
//             Vandegriend.solve(graph, 1000000, (curIter: Int, startTime: Long) => curIter > 1000000)
//         if (recursions > value) {
//             max = graph
//             value = recursions
//         }
//     }


//     printArray(max)
//     println(Vandegriend.solve(max, 1000000, (curIter: Int, startTime: Long) => curIter > 1000000))

//     val edges = Vandegriend.initEdges(max)
//     println(Vandegriend.funcPruneDegreeTwoNeighbours(edges))
// }
