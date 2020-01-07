package graphs

import System._


object PPA extends App {

    val random = scala.util.Random

    def copy(graph: Array[Array[Int]]): Array[Array[Int]] = {
        return graph.map(_.clone)
    }

    def randomMutation(graph: Array[Array[Int]]) = {
        val newGraph = copy(graph)
        val random = scala.util.Random
        val edges = for (
        i <- graph.indices;
        j <- graph.indices;
        if graph(i)(j) == 1 && i < j 
        ) yield (i, j)
        val complement = for (
        i <- 0 until graph.size;
        j <- 0 until graph.size; 
        if !edges.contains((i, j)) && !edges.contains((j, i)) && i != j && i < j
        ) yield (i, j)

        val deleteEdge = edges(random.nextInt(edges.size))
        val newEdge    = complement(random.nextInt(complement.size))

        // 0 -> replace, 1 -> delete, 2 -> add
        val moveType = random.nextInt(3)

        if (moveType == 0) {
        newGraph(deleteEdge._1)(deleteEdge._2) = 0
        newGraph(deleteEdge._2)(deleteEdge._1) = 0
        newGraph(newEdge._1)(newEdge._2) = 1
        newGraph(newEdge._2)(newEdge._1) = 1
        }
        if (moveType == 1) {
        newGraph(deleteEdge._1)(deleteEdge._2) = 0
        newGraph(deleteEdge._2)(deleteEdge._1) = 0
        }
        if (moveType == 2) {
        newGraph(newEdge._1)(newEdge._2) = 1
        newGraph(newEdge._2)(newEdge._1) = 1
        }

        newGraph
    }

    def randomMutations(mutations: Int, graph: Array[Array[Int]]): Array[Array[Int]] =
        if (mutations == 0) graph
        else                randomMutations(mutations - 1, randomMutation(graph))

    //def fitnessMap = (x: Int) = 1/2 * (scala.math.tanh(4*x-2)+1)

    def cutoff(maxTime: Long, maxIter: Int, timeOrIterations: String = "time")
        (curIter: Int, startTime: Long): Boolean =
        if (timeOrIterations == "time") nanoTime - startTime > maxTime
        else                            curIter > maxIter
        
    // def numberOfRunners(nMax: Int, nI: Float): Int
    //     scala.math.ceil(nMax * nI * random.nextFloat)


    // Ni(x) = x / 1,000,000,000 
    // ceil(Ni * maxMutation)
    // def distance()

    def ppa(
        populationSize: Int,
        graphSize: Int,
        amountOfEdges: Int,
        maxRecursions: Int,
    ): (Array[Array[Int]], Int, Boolean) = {
        val l = for (_ <- 0 to populationSize) yield GraphGenerator.genGraph(graphSize, amountOfEdges)
        var p = scala.collection.mutable.ArraySeq(l:_*)
        var bestGraph = p.head
        var maxFitness = 0
        var maxHamiltonian = true

        // .sortWith((a, b) =>  a._2 > b._2)
        var chicke = 0

        for (_ <- 0 to numberOfGens) {
            var n = p.map(graph => CheckAllWithPruningLow.solve(graph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))
            p = scala.collection.mutable.ArraySeq(p.indices.sortWith((a, b) => n(a)._2 > n(b)._2).map(i => p(i)): _*)
            var n_sorted = n.sortWith((a, b) =>  a._2 > b._2)
            // do the top 10 %
            val bound = scala.math.round(0.2 * n.size)
            for (i <- 0 to bound.toInt) {
                for (_ <- 0 to scala.math.round(y / (i + 1)).toInt) {
                    val r = randomMutation(p(i.toInt))
                    val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
                    if (recursions > n(i.toInt)._2) {
                        p(i.toInt) = r
                    }
                    chicke = chicke + 1
                }
            }
            for (i <- (bound + 1) until n.size) {
                val r = randomMutations(k, p(i.toInt))
                val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
                if (recursions > n_sorted(i.toInt)._2 && chicke < maxRecursions) {
                    p(i.toInt) = r
                }
                chicke = chicke + 1
            }
            // do the bottom 90 %
            bestGraph = p.head
            maxFitness = n.head._2
            n_sorted.head._1 match {
                case Some(true) => { maxHamiltonian = true }
                case Some(false) => { maxHamiltonian = false }
                case None => { maxHamiltonian = false}
            }
        }
        println("chicken and fish")
        println(chicke)
        return (bestGraph, maxFitness, maxHamiltonian)
    }
    

    val populationSize = 10
    val numberOfGens = 26
    val graphSize = 16
    val k: Int = 20
    val y = 5
    val experiment_times = 10
    val maxEvaluations = 500

    for (i <- 0 until experiment_times) {
        println(s"Starting ppa $i")
        val (graph, fitness, hamiltonian) = ppa(
            populationSize,
            graphSize,
            31,
            maxEvaluations
        )
        val json = GraphGenerator.graphToJson(i, graph, fitness, hamiltonian)
        GraphGenerator.writeGraphToFile(s"frontend/resources/ppa/$maxEvaluations-evaluations/$graphSize-difficult", i, json)
    }
}

// max - evaluations
