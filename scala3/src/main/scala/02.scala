import scala.io.Source

@main def app: Unit =
  val commands = Source
    .fromURL(getClass.getResource("/02.txt"))
    .getLines()
    .map(line => {
      val Array(command, amount) = line.split(' ');
      (command, amount.toInt)
    })

  final case class State(position: Int, depth: Int, aim: Int)

  val state = commands.foldLeft(State(0, 0, 0)) {
    case (state, ("down", amount)) =>
      state.copy(aim = state.aim + amount)

    case (state, ("up", amount)) =>
      state.copy(aim = state.aim - amount)

    case (state, ("forward", amount)) =>
      state.copy(
        position = state.position + amount,
        depth = state.depth + state.aim * amount,
      )
  }

  println(state.position * state.depth)
