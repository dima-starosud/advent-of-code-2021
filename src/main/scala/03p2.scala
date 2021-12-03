import scala.collection.mutable
import scala.io.Source

def thatFunction(numbers0: Vector[Vector[Char]], leaveZeros: (Int, Int) => Boolean): Int =
  var numbers = numbers0
  var index = 0
  while numbers.size > 1 do
    var zeros = 0
    var ones = 0
    for number <- numbers do
      number(index) match {
        case '0' => zeros += 1
        case '1' => ones += 1
      }
    val leave = if leaveZeros(zeros, ones) then '0' else '1'
    numbers = numbers.filter(_ (index) == leave)
    index += 1

  val Vector(number) = numbers
  Integer.parseInt(number.mkString, 2)


@main def day3p2(): Unit =
  val numbers = Source
    .fromURL(getClass.getResource("/03.txt"))
    .getLines()
    .map(_.toCharArray.toVector)
    .toVector

  val oxygen = thatFunction(numbers, _ > _)
  val co2 = thatFunction(numbers, _ <= _)

  println(oxygen)
  println(co2)
  println(oxygen * co2)
