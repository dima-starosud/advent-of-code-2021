import java.util.Date
import scala.collection.mutable
import scala.util.control.Exception.catching

final class Cave(val name: String):
  var neighbors = Seq.empty[Cave]

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

  var resultPaths = Seq.empty[Seq[Cave]]
  var partialPaths = Seq(Seq(caves("end")))

  while partialPaths.nonEmpty do
    var newPartialPaths = Seq.empty[Seq[Cave]]
    for path <- partialPaths do
      for next <- path.last.neighbors
          path <- tryAppend(path, next) do
        if path.last.name.equals("start") then
          resultPaths :+= path
        else
          newPartialPaths :+= path
    partialPaths = newPartialPaths
    println(s"resultPaths.size=${resultPaths.size}")

  println(resultPaths.size)
  val endTime = System.currentTimeMillis()
  val elapsed = (endTime - startTime) / 1000
  // elapsed = 150 seconds
  println(s"elapsed = $elapsed seconds")


def tryAppend(path: Seq[Cave], next: Cave): Option[Seq[Cave]] =
  val newPath = path :+ next
  if next.name.forall(_.isUpper) then
    Some(newPath)
  else if next.name.forall(_.isLower) then
    val smallCaveNames = newPath
      .map(_.name)
      .filter(_.forall(_.isLower))
      .groupMapReduce(identity)(Function.const(1))(_ + _)

    if smallCaveNames.values.count(_ == 2) <= 1
      && smallCaveNames.values.forall(_ <= 2)
      && smallCaveNames.getOrElse("start", 1) == 1
      && smallCaveNames.getOrElse("end", 1) == 1 then
      Some(newPath)
    else
      None
  else
    throw IllegalArgumentException(next.name)
