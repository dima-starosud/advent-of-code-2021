import scala.io.Source

@main def day3: Unit =
  //  val lines =
  //    """
  //      |00100
  //      |11110
  //      |10110
  //      |10111
  //      |10101
  //      |01111
  //      |00111
  //      |11100
  //      |10000
  //      |11001
  //      |00010
  //      |01010
  //      |""".stripMargin
  //      .linesIterator
  //      .filter(_.nonEmpty)
  //      .map(_.toCharArray.toVector)
  val lines = Source
    .fromURL(getClass.getResource("/03.txt"))
    .getLines()
    .map(_.toCharArray.toVector)

  final case class Sums(var zeros: Int = 0, var ones: Int = 0)

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

  val gammaBits = sumsState
    .map(sums =>
      if sums.zeros > sums.ones then
        '0'
      else if sums.ones > sums.zeros then
        '1'
      else
        throw new RuntimeException("Equal number of bits!")
    )
    .mkString
  val gamma = Integer.parseInt(gammaBits, 2)

  val epsilonBits = sumsState
    .map(sums =>
      if sums.zeros > sums.ones then
        '1'
      else if sums.ones > sums.zeros then
        '0'
      else
        throw new RuntimeException("Equal number of bits!")
    )
    .mkString
  val epsilon = Integer.parseInt(epsilonBits, 2)

  println(sumsState)
  println(gammaBits)
  println(epsilonBits)

  println(gamma * epsilon)
