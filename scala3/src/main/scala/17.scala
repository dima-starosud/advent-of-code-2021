import scala.collection.immutable.NumericRange

final case class Pos(x: Long, y: Long, vx: Long, vy: Long)

def simulateFromZero(vx: Long, vy: Long): Iterator[Pos] =
  Iterator.iterate(Pos(0, 0, vx, vy)) {
    pos =>
      pos.copy(
        x = pos.x + pos.vx,
        y = pos.y + pos.vy,
        vx = if pos.vx != 0 then pos.vx - 1 else 0,
        vy = pos.vy - 1
      )
  }

def checkVelocity(vx: Long, vy: Long,
                  xs: NumericRange.Inclusive[Long], ys: NumericRange.Inclusive[Long]
                 ): Boolean =
  for
    pos <- simulateFromZero(vx, vy)
  do
    if pos.x > xs.max || pos.y < ys.min then
      return false
    if (xs contains pos.x) && (ys contains pos.y) then
      return true
  throw RuntimeException("Impossible")

def countVelocities(xs: NumericRange.Inclusive[Long], ys: NumericRange.Inclusive[Long]): Long =
  (
    for vx <- (0L to xs.max).iterator
        vy <- (ys.min to ys.min.abs).iterator
    yield (vx, vy)
    ).count(checkVelocity(_, _, xs, ys))


@main def day17(): Unit =
  println(countVelocities(20L to 30L, -10L to -5L)) // 112
  println(countVelocities(153L to 199L, -114L to -75L)) // 3186
