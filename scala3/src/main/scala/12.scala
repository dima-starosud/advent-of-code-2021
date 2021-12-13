import java.util.Date
import scala.collection.mutable
import scala.util.control.Exception.catching

final class Cave(val name: String):
  var neighbors = Seq.empty[Cave]

final case class PathStat(last: Cave, counts: Map[String, Int]):
  def tryAppend(next: Cave): Option[PathStat] =
    if next.name.forall(_.isUpper) then
      Some(this.copy(last = next))
    else if next.name.forall(_.isLower) then
      val newPath = this.copy(
        last = next,
        counts = this.counts.updatedWith(next.name)(v => Some(v.getOrElse(0) + 1)),
      )
      if newPath.counts.values.count(_ == 2) <= 1
        && newPath.counts.values.forall(_ <= 2)
        && newPath.counts.getOrElse("start", 1) == 1
        && newPath.counts.getOrElse("end", 1) == 1 then
        Some(newPath)
      else
        None
    else
      throw IllegalArgumentException(next.name)


@main def day12(): Unit =
  val startTime = System.currentTimeMillis()
  val lines_1 =
    """
      |start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end
      |"""
      .stripMargin
      .linesIterator
      .filter(_.nonEmpty)

  val lines =
    """
      |end-ry
      |jf-jb
      |jf-IO
      |jb-hz
      |jo-LM
      |hw-end
      |hw-LM
      |hz-ry
      |WI-start
      |LM-start
      |kd-jf
      |xi-WI
      |hw-jb
      |hz-jf
      |LM-jb
      |jb-xi
      |ry-jf
      |WI-jb
      |end-hz
      |jo-start
      |WI-jo
      |xi-ry
      |xi-LM
      |xi-hw
      |jo-xi
      |WI-jf
      |"""
      .stripMargin
      .linesIterator
      .filter(_.nonEmpty)

  var caves = mutable.Map.empty[String, Cave]
  for line <- lines do
    def cave(name: String) = caves.getOrElseUpdate(name, Cave(name))

    val Array(n1, n2) = line.split('-')
    cave(n1).neighbors :+= cave(n2)
    cave(n2).neighbors :+= cave(n1)

  var resultPaths = Vector.empty[PathStat]
  var partialPaths = Vector(
    PathStat(last = caves("end"), counts = Map("end" -> 1))
  )

  while partialPaths.nonEmpty do
    var newPartialPaths = Vector.empty[PathStat]
    for path <- partialPaths do
      for next <- path.last.neighbors
          path <- path.tryAppend(next) do
        if path.last.name.equals("start") then
          resultPaths :+= path
        else
          newPartialPaths :+= path
    partialPaths = newPartialPaths

  // 155477
  println(resultPaths.size)
  val endTime = System.currentTimeMillis()
  val elapsed = endTime - startTime
  // elapsed = 832 millis
  println(s"elapsed = $elapsed millis")
