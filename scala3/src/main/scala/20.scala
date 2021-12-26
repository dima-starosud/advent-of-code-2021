import scala.io.Source


enum Cell:
  case Hash, Dot


object Cell:
  val parse: Char => Cell = {
    case '#' => Hash
    case '.' => Dot
  }

  extension (cell: Cell)
    def toBit: Char = cell match {
      case Hash => '1'
      case Dot => '0'
    }
    def toChar: Char = cell match {
      case Hash => '#'
      case Dot => '.'
    }


type Algorithm = Vector[Cell]
type Field = Map[(Int, Int), Cell]


extension (field: Field)
  def toFieldString: String =
    val (minRowNum, minColNum) = field.keys.min
    val (maxRowNum, maxColNum) = field.keys.max
    (minRowNum to maxRowNum)
      .map { row =>
        (minColNum to maxColNum)
          .map(col => field((row, col)).toChar)
          .mkString
      }
      .mkString("\n")


def extend(field: Field)(replace: (Int, Int) => Cell): Field =
  val (minRowNum, minColNum) = field.keys.min
  val (maxRowNum, maxColNum) = field.keys.max
  Map.from(
    for rownum <- minRowNum - 1 to maxRowNum + 1
        colnum <- minColNum - 1 to maxColNum + 1
    yield
      (rownum, colnum) -> replace(rownum, colnum)
  )


def evolve(field: Field, algorithm: Algorithm): Field =
  val default: Cell = field(field.keys.min)
  extend(field) { (rownum, colnum) =>
    val binary = (
      for dR <- -1 to 1
          dC <- -1 to 1
      yield field.getOrElse((rownum + dR, colnum + dC), default).toBit
      )
      .mkString
      .toBinaryInt

    algorithm(binary)
  }


def border(field: Field, border: Cell): Field =
  extend(field)((rownum, colnum) => field.getOrElse((rownum, colnum), border))


@main def day20(): Unit =
  val algorithm_1: Algorithm =
    """
      |..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
      |#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
      |.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
      |.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
      |.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
      |...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
      |..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
      |"""
      .stripMargin
      .strip()
      .linesIterator
      .flatten
      .map(Cell.parse)
      .toVector

  val raw_1 =
    """
      |...............
      |...............
      |...............
      |...............
      |.....#..#......
      |.....#.........
      |.....##..#.....
      |.......#.......
      |.......###.....
      |...............
      |...............
      |...............
      |...............
      |"""
      .stripMargin
      .strip()
      .linesIterator

  val raw =
    Source
      .fromURL(getClass.getResource("/20.txt"))
      .getLines()

  val algorithm = raw.next().map(Cell.parse).toVector
  raw.next()

  var field: Field = Map.from(
    for {
      (line, rownum) <- raw.zipWithIndex
      (cell, colnum) <- line.zipWithIndex
    } yield {
      (rownum, colnum) -> Cell.parse(cell)
    })

  field = border(field, Cell.Dot)
  for _ <- 1 to 50 do
    field = evolve(field, algorithm)

  println(field.values.count(_ == Cell.Hash))
