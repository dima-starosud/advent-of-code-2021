import org.jgrapht.alg.interfaces.ShortestPathAlgorithm
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultWeightedEdge}

import scala.io.Source
import scala.util.control.Exception.catching


@main def day15(): Unit =
  val lines_1 =
    """
      |1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581
      |"""
      .stripMargin
      .strip()
      .linesIterator

  val lines = Source
    .fromURL(getClass.getResource("/15.txt"))
    .getLines()

  type Node = (Int, Int)
  type Matrix = Vector[Vector[Int]]

  val matrix: Matrix = {
    val matrix0 =
      lines
        .map(_.map(_.toString.toInt).toVector)
        .toVector

    def increase(cost: Int): Int =
      val newCost = cost + 1
      if newCost > 9 then 1 else newCost

    val matrix1 =
      matrix0.map(
        Vector.iterate(_, 5)(_.map(increase)).flatten)

    val matrix2 =
      Vector.iterate(matrix1, 5)(_.map(_.map(increase))).flatten

    matrix2
  }

  val source = (0, 0)
  val target = (matrix.indices.last, matrix(0).indices.last)

  val graph = DefaultDirectedWeightedGraph[Node, DefaultWeightedEdge](classOf[DefaultWeightedEdge])

  for
    r <- matrix.indices
    c <- matrix(0).indices
  do
    graph.addVertex((r, c))

  for
    r <- matrix.indices
    c <- matrix(0).indices
    (dr, dc) <- Seq((-1, 0), (0, -1), (1, 0), (0, 1))
    r1 = r + dr
    c1 = c + dc
    cost <- catching(classOf[IndexOutOfBoundsException]).opt {
      matrix(r1)(c1)
    }
  do
    val edge = graph.addEdge((r, c), (r1, c1))
    graph.setEdgeWeight(edge, cost)

  val paths = DijkstraShortestPath(graph).getPaths(source)

  println(paths.getWeight(target))
