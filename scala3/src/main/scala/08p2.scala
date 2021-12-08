import scala.collection.mutable
import scala.io.Source

type Digit = Set[Char]
type Mapping = Map[Digit, Int]

def memo[A, B](f: A => B): (A => B) =
  val cache = mutable.Map.empty[A, B]
  x => cache.getOrElseUpdate(x, f(x))

def getMapping(numbers0: Seq[Digit]): Mapping =
  var numbers = numbers0
  val Seq(one) = numbers.filter(_.size == 2)
  val Seq(four) = numbers.filter(_.size == 4)
  val Seq(seven) = numbers.filter(_.size == 3)
  val Seq(eight) = numbers.filter(_.size == 7)

  val Seq(six) = numbers.filter(digit => digit.size == 6 && !(one subsetOf digit))
  val Seq(five) = numbers.filter(digit => digit.size == 5 && (digit subsetOf six))

  numbers = (numbers.toSet -- Set(one, four, seven, eight, six, five)).toSeq

  val (Seq(three), Seq(two)) = numbers.filter(_.size == 5).partition(one subsetOf _)
  val (Seq(nine), Seq(zero)) = numbers.filter(_.size == 6).partition(four subsetOf _)

  Map(
    zero -> 0,
    one -> 1,
    two -> 2,
    three -> 3,
    four -> 4,
    five -> 5,
    six -> 6,
    seven -> 7,
    eight -> 8,
    nine -> 9
  )


@main def day8p2(): Unit =
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

  val getMappingMemo = memo(getMapping)

  val solution = lines
    .map { line =>
      val Array(input, output) = line
        .split('|')
        .map(_.strip().split(' ').map(_.toSet).toSeq)
      val mapping = getMappingMemo(input)
      output
        .map(mapping)
        .mkString
        .toInt
    }
    .sum

  println(solution)
