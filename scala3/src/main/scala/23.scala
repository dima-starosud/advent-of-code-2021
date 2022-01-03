import scala.collection.mutable
import scala.util.control.Exception.catching


@main def day23(): Unit =
  val baseRaw =
    """
      |#############
      |#..X.X.X.X..#
      |###A#B#C#D###
      |  #A#B#C#D#
      |  #A#B#C#D#
      |  #A#B#C#D#
      |  #########
      |""".stripMargin.strip()

  val inputRaw_1 =
    """
      |#############
      |#...........#
      |###B#C#B#D###
      |  #D#C#B#A#
      |  #D#B#A#C#
      |  #A#D#C#A#
      |  #########
      |""".stripMargin.strip()

  val inputRaw =
    """
      |#############
      |#...........#
      |###B#B#D#D###
      |  #D#C#B#A#
      |  #D#B#A#C#
      |  #C#A#A#C#
      |  #########
      |""".stripMargin.strip()

  enum AP:
    case A, B, C, D

  enum Cell:
    case Wall, Hallway, Entrance, Empty
    case Room(ap: AP)

  object Cell:
    def parse: Char => Cell = {
      case '.' => Hallway
      case ' ' => Empty
      case 'X' => Entrance
      case '#' => Wall
      case ap => Room(AP.valueOf(ap.toString))
    }

  val baseWorld = (
    for {
      (row, rownum) <- baseRaw.linesIterator.zipWithIndex
      (cell, colnum) <- row.zipWithIndex
    } yield {
      (rownum, colnum) -> Cell.parse(cell)
    }).toMap

  type Pos = (Int, Int)
  type Poses = Map[Pos, AP]

  val initialPoses: Poses = (
    for {
      (row, rownum) <- inputRaw.linesIterator.zipWithIndex
      (cell, colnum) <- row.zipWithIndex
      x <- catching(classOf[IllegalArgumentException]).opt {
        AP.valueOf(cell.toString)
      }
    } yield {
      (rownum, colnum) -> x
    }).toMap

  val hallway = baseWorld.toSeq.collect { case (pos, Cell.Hallway) => pos }

  val to = baseWorld.collect { case (key, Cell.Room(ap)) => key -> ap }

  val sortedTo =
    to.toSeq
      .groupBy(_._2)
      .values.map(_.sortBy(_._1).reverse)
      .toSeq

  val destinations =
    to.toSeq
      .groupBy(_._2)
      .view.mapValues(_.map(_._1).sorted.reverse)
      .toMap

  val cost = Map(
    AP.A -> 1,
    AP.B -> 10,
    AP.C -> 100,
    AP.D -> 1000,
  )

  val Seq(hallwayRownum) = hallway.map(_._1).distinct

  def movePath: (Pos, Pos) => Seq[Pos] = {
    case ((currRownum, currColnum), (destRownum, destColnum)) =>
      val toHallway = (currRownum until hallwayRownum by -1).map(_ -> currColnum)
      val onHallway = (currColnum until destColnum by (if currColnum > destColnum then -1 else 1)).map(hallwayRownum -> _)
      val toDestination = (hallwayRownum to destRownum).map(_ -> destColnum)
      (toHallway ++ onHallway ++ toDestination).drop(1)
  }

  val cache = mutable.Map.empty[Poses, Option[Long]]

  def minEnergy(from: Poses): Option[Long] =
    def validPath(curr: Pos, dest: Pos): Option[Seq[Pos]] =
      val path = movePath(curr, dest)
      if path exists from.contains then None
      else Some(path)

    cache.getOrElseUpdate(from, {
      val fromSet = from.toSet
      val movables = fromSet -- sortedTo.flatMap(_.takeWhile(fromSet contains _))
      if movables.isEmpty then
        Some(0L)
      else
        (
          for {
            (curr@(currRownum, _), ap) <- movables.toSeq
            highPriorityPath =
              destinations(ap)
                .dropWhile(from.get(_).contains(ap))
                .headOption
                .flatMap(validPath(curr, _))
                .map(Iterator(_))
            hallwayPaths =
              if currRownum == hallwayRownum then None
              else Some(hallway.iterator.flatMap(validPath(curr, _)))
            path <- (highPriorityPath orElse hallwayPaths).iterator.flatten
          } yield {
            minEnergy((from - curr) + (path.last -> ap)).map(_ + path.size * cost(ap))
          })
          .flatten
          .minOption
    })

  println(minEnergy(initialPoses)) // Some(59071)
