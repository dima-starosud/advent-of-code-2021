import scala.io.Source

@main def day13(): Unit =
  val lines_1 =
    """
      |6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5
      |"""
      .stripMargin
      .strip()
      .linesIterator

  val lines = Source
    .fromURL(getClass.getResource("/13.txt"))
    .getLines()

  val dots =
    lines
      .takeWhile(_.nonEmpty)
      .map { line =>
        val Array(x, y) = line.split(',').map(_.toInt)
        (x, y)
      }
      .toSet

  enum Fold:
    case X, Y

  val folds =
    lines
      .map(_.split('=') match {
        case Array("fold along x", x) => Fold.X -> x.toInt
        case Array("fold along y", y) => Fold.Y -> y.toInt
      })
      .toSeq

  val dots1 = folds.foldLeft(dots) {
    case (dots, (Fold.X, theX)) =>
      val xMax = 2 * theX
      dots.collect {
        case (x, y) if x < theX => (x, y)
        case (x, y) if x > theX => (xMax - x, y)
      }
    case (dots, (Fold.Y, theY)) =>
      val yMax = 2 * theY
      dots.collect {
        case (x, y) if y < theY => (x, y)
        case (x, y) if y > theY => (x, yMax - y)
      }
  }

  val maxX = dots1.map(_._1).max
  val maxY = dots1.map(_._2).max
  val result = (0 to maxY)
    .map(y =>
      (0 to maxX)
        .map(_ -> y)
        .map(dot => if dots1 contains dot then '#' else ' ')
        .mkString
    )
    .mkString("\n")
  println(result) // GJZGLUPJ
