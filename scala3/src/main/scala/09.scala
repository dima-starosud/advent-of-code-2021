import java.util.NoSuchElementException
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

case object Edge

final class Slope(val height: Int):
  var basinId = Option.empty[Int]
  var neighbors = List.empty[Slope]

type Basin = Edge.type | Slope

object Basin:
  def apply(height: Int): Basin =
    if height == 9 then
      Edge
    else
      Slope(height)


@main def day9(): Unit =
  val lines_1 =
    """
      |2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678
      |"""
      .stripMargin
      .linesIterator
      .filter(_.nonEmpty)

  val lines = Source
    .fromURL(getClass.getResource("/09.txt"))
    .getLines()

  val basins: Seq[Seq[Basin]] =
    lines
      .map(_
        .map(_.toString.toInt)
        .map[Basin](Basin(_))
      )
      .toSeq

  for Seq(row1, row2) <- basins.sliding(2)
      (s1: Slope, s2: Slope) <- row1 zip row2 do
    s1.neighbors +:= s2
    s2.neighbors +:= s1

  for row <- basins
      Seq(s1: Slope, s2: Slope) <- row.sliding(2) do
    s1.neighbors +:= s2
    s2.neighbors +:= s1

  val slopes: Seq[Slope] = basins.flatten.collect { case s: Slope => s }

  var basinId = 0

  for slope <- slopes
      if slope.basinId.isEmpty do
    basinId += 1
    slope.basinId = Some(basinId)
    var recursive = List(slope)
    while recursive.nonEmpty do
      val next = recursive.head
      recursive = next.neighbors.filter(_.basinId.isEmpty).tapEach(_.basinId = Some(basinId))
        ++ recursive.tail

  val solution =
    slopes
      .flatMap(_.basinId)
      .groupMapReduce(identity)(Function.const(1))(_ + _)
      .map(_._2)
      .foldLeft(Seq(1, 1, 1))(_.appended(_).sorted.drop(1))
      .product

  println(solution) // 949905
