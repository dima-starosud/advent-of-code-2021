import scala.annotation.tailrec
import scala.io.Source
import scala.util.control.Exception.catching

enum Bracket:
  case Round, Square, Curly, Angle

final case class Unmatched(closing: Seq[Bracket] = Nil, opening: Seq[Bracket] = Nil)

object Unmatched:

  import Bracket.*

  def parse(line: String): Seq[Unmatched] = line map {
    case '(' => Unmatched(opening = Seq(Round))
    case ')' => Unmatched(closing = Seq(Round))
    case '[' => Unmatched(opening = Seq(Square))
    case ']' => Unmatched(closing = Seq(Square))
    case '{' => Unmatched(opening = Seq(Curly))
    case '}' => Unmatched(closing = Seq(Curly))
    case '<' => Unmatched(opening = Seq(Angle))
    case '>' => Unmatched(closing = Seq(Angle))
  }

  final case class CorruptedBracket(opening: Bracket, closing: Bracket) extends Exception

  def append(u1: Unmatched, u2: Unmatched): Unmatched =
    // TODO @tailrec
    def cancel: (Seq[Bracket], Seq[Bracket]) => (Seq[Bracket], Seq[Bracket]) = {
      case (b +: bs, c +: cs) =>
        if (b != c) throw CorruptedBracket(b, c)
        cancel(bs, cs)
      case (bs, cs) => (bs, cs)
    }

    val (u1Opening, u2Closing) = cancel(u1.opening, u2.closing)
    Unmatched(u1.closing ++ u2Closing, u2.opening ++ u1Opening)


@main def day10(): Unit =
  val lines_1 =
    """
      |[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]
      |"""
      .stripMargin
      .linesIterator
      .filter(_.nonEmpty)

  val lines = Source
    .fromURL(getClass.getResource("/10.txt"))
    .getLines()

  val SCORE = Map(
    Bracket.Round -> 1,
    Bracket.Square -> 2,
    Bracket.Curly -> 3,
    Bracket.Angle -> 4,
  )

  val sorted_scores =
    lines
      .flatMap { line =>
        catching(classOf[Unmatched.CorruptedBracket]).opt {
          Unmatched.parse(line).foldLeft(Unmatched())(Unmatched.append)
        }
      }
      .map {
        case Unmatched(Nil, opening) =>
          opening.foldLeft(0.toLong)(_ * 5 + SCORE(_))
      }
      .toSeq
      .sorted

  println(sorted_scores(sorted_scores.size / 2))
