import scala.collection.mutable
import scala.io.Source

@main def day14(): Unit =
  val lines_12345 =
    """
      |NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C
      |"""
      .stripMargin
      .strip()
      .linesIterator

  val lines = Source
    .fromURL(getClass.getResource("/14.txt"))
    .getLines()

  val template = lines.next()

  val rules =
    lines
      .drop(1)
      .map { line =>
        val Array(Array(first, second), Array(single)) = line.split(" -> ").map(_.toCharArray)
        (first, second) -> single
      }
      .toMap

  type Counts = Map[Char, BigInt]

  def naive(depth: Int): (String, Counts) = {
    def step(polymer: Seq[Char]): Vector[Char] =
      polymer.head +: polymer
        .iterator
        .sliding(2)
        .flatMap(_ match { case s@Seq(x, y) => rules.get((x, y)).map(m => Seq(m, y)).getOrElse(Seq(y)) })
        .toVector

    val result = Seq.fill(depth)(step).foldRight(template.toVector)(_ (_))
    result.mkString -> result.groupMapReduce(identity)(Function.const(BigInt(1)))(_ + _)
  }

  def merge(counts: Counts*): Counts =
    counts
      .flatMap(_.keys)
      .toSet
      .map { key =>
        key -> counts.map(_.getOrElse(key, BigInt(0))).sum
      }
      .toMap

  val cache = mutable.Map.empty[(Char, Char, Int), Counts]

  def recursive(first: Char, second: Char, depth: Int): Counts =
    cache.get((first, second, depth)) match {
      case None =>
      case Some(counts) =>
        return counts
    }

    val result: Counts =
      if depth > 0 then
        rules.get(first -> second) match {
          case None =>
            merge(Map(first -> 1), Map(second -> 1))
          case Some(middle) =>
            merge(
              recursive(first, middle, depth - 1),
              recursive(middle, second, depth - 1),
              Map(middle -> -1),
            )
        }
      else
        merge(Map(first -> 1), Map(second -> 1))

    cache += (first, second, depth) -> result
    result

  def fast(depth: Int): Counts = {
    val countsPlus =
      template
        .iterator
        .sliding(2)
        .map { case Seq(f, s) => recursive(f, s, depth) }
        .toSeq

    val countsMinus =
      template.init.tail.map(c => Map(c -> BigInt(-1)))

    merge((countsMinus ++ countsPlus) *)
  }

  def diffQuants(counts: Counts): BigInt =
    val sortedCounts =
      counts
        .toSeq
        .sortBy(_._2)
    sortedCounts.last._2 - sortedCounts.head._2

  println(diffQuants(fast(40)))
