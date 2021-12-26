import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}


final case class Player(position: Int, score: Int):
  def go(outcome: Int): Player =
    var newPosition = position + outcome
    if newPosition > 10 then
      newPosition = newPosition % 10
      if newPosition == 0 then
        newPosition = 10
    val newScore = score + newPosition
    Player(newPosition, newScore)


final case class Game(player1: Player, player2: Player, turn: 1 | 2 = 1):
  val Outcomes: Seq[Int] =
    for r1 <- (1 to 3)
        r2 <- (1 to 3)
        r3 <- (1 to 3)
    yield r1 + r2 + r3

  def go: Seq[Game] =
    turn match {
      case 1 =>
        Outcomes.map(outcome => copy(player1 = player1.go(outcome), turn = 2))
      case 2 =>
        Outcomes.map(outcome => copy(player2 = player2.go(outcome), turn = 1))
    }


@main def day21(): Unit =
  var pendingGames: Map[Game, Long] = Map(
    Game(Player(10, 0), Player(2, 0)) -> 1L
  )

  var player1WinCount = 0L
  var player2WinCount = 0L

  var counter = 0

  while pendingGames.nonEmpty do
    val (game, count) = pendingGames.head
    pendingGames = pendingGames - game

    if game.player1.score >= 21 then
      player1WinCount += count
    else if game.player2.score >= 21 then
      player2WinCount += count
    else
      for g1 <- game.go do
        pendingGames = pendingGames.updatedWith(g1) {
          oldCount => Some(oldCount.getOrElse(0L) + count)
        }

    counter += 1
    if counter == 1000000 then
      counter = 0
      println("---------------- Iteration ----------------")
      println(s"pendingGames: ${pendingGames.size}")
      println(s"player1WinCount: $player1WinCount")
      println(s"player2WinCount: $player2WinCount")

  println("---------------- Result ----------------")
  println(s"player1WinCount: $player1WinCount")
  println(s"player2WinCount: $player2WinCount")


@main def day21Recursive(): Unit =
  type Result = (Long, Long)
  val cache = mutable.Map.empty[Game, Result]

  def recursive(game: Game): Result =
    cache.get(game) match {
      case None =>
      case Some(result) =>
        return result
    }

    val result: Result =
      if game.player1.score >= 21 then
        (1, 0)
      else if game.player2.score >= 21 then
        (0, 1)
      else
        val (p1, p2) = game.go.map(recursive).unzip
        (p1.sum, p2.sum)

    cache(game) = result
    result

  val (player1WinCount, player2WinCount) = recursive(Game(Player(10, 0), Player(2, 0)))
  println("---------------- Result ----------------")
  println(s"player1WinCount: $player1WinCount")
  println(s"player2WinCount: $player2WinCount")
