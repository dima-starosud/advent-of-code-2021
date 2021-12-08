import scala.collection.mutable
import scala.io.Source

object day08BFUtils:
  val BaseMapping: Map[String, Int] = Map(
    "abcefg" -> 0,
    "cf" -> 1,
    "acdeg" -> 2,
    "acdfg" -> 3,
    "bcdf" -> 4,
    "adbfg" -> 5,
    "abdefg" -> 6,
    "acf" -> 7,
    "abcdefg" -> 8,
    "abcdfg" -> 9,
  )

  val Chars = "abcdegf"

  type Digit = Set[Char]

  val Permutations: Map[Set[Digit], Map[Digit, Int]] = Chars
    .permutations
    .map(_
      .zipWithIndex
      .toMap
      .view
      .mapValues(Chars(_))
      .toMap)
    .map(permToDigits)
    .map(map => map.keySet -> map)
    .toMap

  def permToDigits(perm: Map[Char, Char]): Map[Digit, Int] =
    BaseMapping
      .map((k, v) => {
        k.map(perm).toSet -> v
      })


@main def day8p2BF(): Unit =
  val lines_1 =
    """
      |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
      |"""
      .stripMargin
      .linesIterator
      .filter(_.nonEmpty)
      .toVector

  val lines = Source
    .fromURL(getClass.getResource("/08.txt"))
    .getLines()
    .toVector

  val solution = lines
    .map { line =>
      val Array(input, output) = line
        .split('|')
        .map(_.strip().split(' ').map(_.toSet))
      val permutation = day08BFUtils.Permutations(input.toSet)
      output
        .map(permutation)
        .mkString
        .toInt
    }
    .sum

  println(solution)
