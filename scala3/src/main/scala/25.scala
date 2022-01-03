package day25

import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

@main def day25(): Unit =
  val input_1 =
    """
      |v...>>.vv>
      |.vv>>.vv..
      |>>.>v>...v
      |>>v>>.>.v.
      |v>v.vv.v..
      |>.>>..v...
      |.vv..>.>v.
      |v.v..>>v.v
      |....v..v.>
      |"""
      .stripMargin
      .strip()
      .linesIterator
      .toVector

  val input = Source
    .fromURL(getClass.getResource("/25.txt"))
    .getLines()
    .toVector

  type Cell = 'v' | '>' | '.'

  val state = Array.fill[Cell](input.size, input(0).size)('.')

  for {
    (line, rownum) <- input.zipWithIndex
    (cell, colnum) <- line.zipWithIndex
  } do {
    state(rownum)(colnum) = cell match {
      case c: Cell => c
    }
  }

  def move(cell: Cell, next: (Int, Int) => (Int, Int)): Boolean =
    var updates =
      for rownum <- state.indices
          colnum <- state(0).indices
          if state(rownum)(colnum) == cell
          (nextRownum, nextColnum) = next(rownum, colnum)
          if state(nextRownum)(nextColnum) == '.'
      yield () => {
        state(rownum)(colnum) = '.'
        state(nextRownum)(nextColnum) = cell
      }

    updates.foreach(_ ())
    updates.nonEmpty

  var step = 0L
  breakable {
    while true do
      step += 1
      val `move->` = move('>', (rn, cn) => (rn, (1 + cn) % state(0).size))
      val `move-v` = move('v', (rn, cn) => ((1 + rn) % state.size, cn))
      if !`move->` && !`move-v` then
        break
  }

  println(step) // 504
