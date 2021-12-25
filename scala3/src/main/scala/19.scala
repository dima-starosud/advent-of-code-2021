import Geometry.*
import org.apache.commons.geometry.euclidean.threed.rotation.{QuaternionRotation, Rotation3D}
import org.apache.commons.geometry.euclidean.threed.{AffineTransformMatrix3D, Vector3D}
import org.apache.commons.numbers.quaternion.Quaternion

import scala.io.Source
import scala.util.control.Exception.catching

object Geometry:

  import math.Numeric.Implicits.infixNumericOps

  extension[X: Numeric] (xs: Array[X])
    def closeTo[Y: Numeric](ys: Array[Y]): Boolean =
      (xs zip ys)
        .map(_.toDouble - _.toDouble)
        .map(_.abs)
        .forall(_ < 1E-6)

  extension (qr1: QuaternionRotation)
    def closeTo(qr2: QuaternionRotation): Boolean =
      qr1.toMatrix.toArray closeTo qr2.toMatrix.toArray

  extension (point: Point)
    def toVector3D: Vector3D =
      val Vector(x, y, z) = point
      Vector3D.of(x, y, z)

  extension (vector: Vector3D)
    def toPointOpt: Option[Point] =
      val result = vector.toArray.map(math.round)
      if result closeTo vector.toArray then Some(result.toVector) else None

  def unique(rotations: Seq[QuaternionRotation]): Seq[QuaternionRotation] =
    var result = Vector.empty[QuaternionRotation]
    for rot <- rotations
        if result.forall(res => !(rot closeTo res))
    do result :+= rot
    result

  lazy val rotations: Seq[QuaternionRotation] =
    val basic =
      unique(
        for axis <- Seq(Vector3D.Unit.PLUS_X, Vector3D.Unit.PLUS_Y, Vector3D.Unit.PLUS_Z)
            q <- Seq(0, 1, 2, 3)
            angle = q * Math.PI / 2
            rot = QuaternionRotation.fromAxisAngle(axis, angle)
        yield
          QuaternionRotation.fromAxisAngle(axis, angle)
      )

    unique(
      for size <- 1 to basic.size
          rot +: rots <- basic.combinations(size)
      yield rots.foldLeft(rot)(_ multiply _)
    )


type Point = Vector[Long]
type Scanner = Seq[Point]
type Matched = Scanner

def matchScanners(matchWith: Scanner, pendingScanner: Scanner): Option[(Vector3D, Scanner)] =
  val matched =
    for matchedPoint <- matchWith.iterator
        pendingPoint <- pendingScanner.iterator
        rotation <- Geometry.rotations.iterator
        rotatedPendingPoint <- rotation(pendingPoint.toVector3D).toPointOpt
        movingVector = matchedPoint.toVector3D subtract rotatedPendingPoint.toVector3D
        affine = rotation.toMatrix.premultiply(AffineTransformMatrix3D.createTranslation(movingVector))
        affinedPendingScanner = pendingScanner.map(p => affine(p.toVector3D).toPointOpt.get)
        if (affinedPendingScanner.toSet intersect matchWith.toSet).size >= 12
    yield
      affine(Vector3D.of(0, 0, 0)) -> affinedPendingScanner
  matched.nextOption()

@main def day19(): Unit =
  val input: Vector[Scanner] =
    Source
      .fromURL(getClass.getResource("/19.txt"))
      .getLines()
      .mkString("\n")
      .split(raw"\n\n")
      .map(_
        .split('\n')
        .drop(1)
        .map(_.split(',').map(_.toLong).toVector)
        .toVector
      )
      .toVector

  var mergedPoints: Set[Point] = input.head.toSet
  var matchedScanners: Seq[Vector3D] = Seq(Vector3D.of(0, 0, 0))

  var matchWithScanners: Seq[Scanner] = Seq(input.head)
  var pendingScanners: Seq[Scanner] = input.tail

  while pendingScanners.nonEmpty do
    println(s"Pending scanners count: ${pendingScanners.size}")
    var newPendingScanners: Seq[Scanner] = Seq.empty
    var newMatchWithScanners: Seq[Scanner] = Seq.empty
    for pendingScanner <- pendingScanners do
      print(".")
      val matched =
        matchWithScanners
          .iterator
          .flatMap(matchScanners(_, pendingScanner))
          .nextOption()
      matched match {
        case None =>
          newPendingScanners :+= pendingScanner
        case Some((matchedScanner, matchedPoints)) =>
          mergedPoints ++= matchedPoints
          matchedScanners :+= matchedScanner
          newMatchWithScanners :+= matchedPoints
      }

    pendingScanners = newPendingScanners
    matchWithScanners = newMatchWithScanners
    println()

  println()
  println(s"Points count: ${mergedPoints.size}")

  val distances =
    for s1 <- matchedScanners
        s2 <- matchedScanners
    yield
      (s1 subtract s2).toArray.map(_.abs).sum
  println(s"Distance: ${distances.max}")
