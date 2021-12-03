import scala.io.Source

final case class Sums(var zeros: Int = 0, var ones: Int = 0)

def toDry(sumsState: Vector[Sums], ifZeros: Char, ifOnes: Char): Int =
  val gammaBits = sumsState
    .map(sums =>
      if sums.zeros > sums.ones then
        ifZeros
      else if sums.ones > sums.zeros then
        ifOnes
      else
        throw new RuntimeException("Equal number of bits!")
    )
    .mkString
  Integer.parseInt(gammaBits, 2)


@main def day3p1(): Unit =
  val lines1 =
    """
      |00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010
      |""".stripMargin
      .linesIterator
      .filter(_.nonEmpty)
      .map(_.toCharArray.toVector)
  val lines = Source
    .fromURL(getClass.getResource("/03.txt"))
    .getLines()
    .map(_.toCharArray.toVector)

  var sumsState = Vector.empty[Sums]

  lines.foreach(bits => {
    while sumsState.length < bits.length do
      sumsState = sumsState.appended(Sums())

    bits.zipWithIndex.foreach {
      case ('0', index) =>
        sumsState(index).zeros += 1
      case ('1', index) =>
        sumsState(index).ones += 1
    }
  })

  val gamma = toDry(sumsState, '0', '1')
  val epsilon = toDry(sumsState, '1', '0')

  println(sumsState)
  println(gamma)
  println(epsilon)

  println(gamma * epsilon)
