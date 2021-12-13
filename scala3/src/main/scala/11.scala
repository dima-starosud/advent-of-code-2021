import scala.util.control.Exception.catching

final class Octopus(var energy: Int):
  var neighbors = Seq.empty[Octopus]

@main def day11(): Unit =
  val lines =
    """
      |3322874652
      |5636588857
      |7755117548
      |5854121833
      |2856682477
      |3124873812
      |1541372254
      |8634383236
      |2424323348
      |2265635842
      |"""
      .stripMargin
      .linesIterator
      .filter(_.nonEmpty)

  val octopusesMx: Seq[Seq[Octopus]] =
    lines
      .map(_
        .map(_.toString.toInt)
        .map(Octopus(_))
      )
      .toSeq

  for r <- octopusesMx.indices
      c <- octopusesMx(0).indices do
    octopusesMx(r)(c).neighbors =
      for {
        dr <- -1 to 1
        dc <- -1 to 1
        if (dr, dc) != (0, 0)
        o <- catching(classOf[IndexOutOfBoundsException]).opt {
          octopusesMx(r + dr)(c + dc)
        }
      } yield {
        o
      }

  val octopuses = octopusesMx.flatten
  var step = 0
  while true do
    step += 1

    var pendingOctopuses = octopuses
    while pendingOctopuses.nonEmpty do
      pendingOctopuses.foreach(_.energy += 1)
      pendingOctopuses =
        for octopus <- pendingOctopuses.distinct
            if octopus.energy > 9
            neighbor <- octopus.neighbors
            if neighbor.energy <= 9
        yield
          neighbor

    for octopus <- octopuses
        if octopus.energy > 9 do
      octopus.energy = 0

    if octopuses.forall(_.energy == 0) then
      println(step)
      return
