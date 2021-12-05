import scala.io.Source

@main def hello: Unit =
  val numbers = Source
    .fromURL(getClass.getResource("/01.txt"))
    .getLines()
    .map(_.toInt)

  val sums = numbers
    .scanRight(Seq.empty[Int])(_ +: _ take 3)
    .filter(_.size == 3)
    .map(_.sum)

  final case class State(previous: Int, count: Int)

  val State(_, count) = sums.foldLeft(State(Int.MaxValue, 0))(
    (state, value) =>
      state.copy(
        previous = value,
        count = (if (value > state.previous) 1 else 0) + state.count)
  )

  println(count)
