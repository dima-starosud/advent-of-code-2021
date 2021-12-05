import scala.collection.mutable
import scala.io.Source

type Board = Vector[mutable.Seq[Int]]

extension (board: Board)
  def set(number: Int) =
    for row <- board do
      for index <- row.indices do
        if row(index) == number then
          row(index) = Int.MinValue

  def isWinner: Boolean =
    for i <- 0 to 4 do
      if (0 to 4).forall(j => board(i)(j) == Int.MinValue) then
        return true
      if (0 to 4).forall(j => board(j)(i) == Int.MinValue) then
        return true
    return false

  def sumOfUnset: Int =
    board.flatten.filter(_ != Int.MinValue).sum

@main def day4(): Unit =
  val lines = Source
    .fromURL(getClass.getResource("/04.txt"))
    .getLines()

  val lines1 =
    """|7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
       |
       |22 13 17 11  0
       | 8  2 23  4 24
       |21  9 14 16  7
       | 6 10  3 18  5
       | 1 12 20 15 19
       |
       | 3 15  0  2 22
       | 9 18 13 17  5
       |19  8  7 25 23
       |20 11 10 24  4
       |14 21 16 12  6
       |
       |14 21 17 24  4
       |10 16 15  9 19
       |18  8 23 26 20
       |22 11 13  6  5
       | 2  0 12  3  7
       |""".stripMargin
      .linesIterator

  val numbers = lines
    .next()
    .split(',')
    .map(_.toInt)
    .toVector

  var boards: Vector[Board] = lines
    .grouped(6)
    .map(_
      .map(_
        .split(raw"\s+")
        .filter(_.nonEmpty)
        .map(_.toInt))
      .map(mutable.Seq.from)
      .filter(_.nonEmpty)
      .toVector)
    .toVector

  var lastBoard = Int.MinValue
  for number <- numbers do
    var newBoards = Vector.empty[Board]
    for board <- boards do
      board.set(number)
      if board.isWinner then
        lastBoard = board.sumOfUnset * number
      else
        newBoards :+= board
    boards = newBoards

  println(lastBoard)
