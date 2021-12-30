import scala.collection.immutable.NumericRange
import scala.collection.mutable
import scala.io.Source
import scala.util.Random
import scala.util.parsing.combinator.RegexParsers


enum Reg:
  case w, x, y, z


type RightV = Either[Reg, Int]


enum Stmt:
  case inp(v: Reg)
  case add(a: Reg, b: RightV)
  case mul(a: Reg, b: RightV)
  case div(a: Reg, b: RightV)
  case mod(a: Reg, b: RightV)
  case eql(a: Reg, b: RightV)
  case load(a: Reg, b: RightV)


type Program = Seq[Stmt]


object Program:
  object Parser extends RegexParsers :
    def stmt: Parser[Stmt] =
      import Stmt.*
      ("inp" ~> left ^^ inp.apply)
        | ("add" ~> left ~ right ^^ { case l ~ r => add(l, r) })
        | ("mul" ~> left ~ right ^^ { case l ~ r => mul(l, r) })
        | ("div" ~> left ~ right ^^ { case l ~ r => div(l, r) })
        | ("mod" ~> left ~ right ^^ { case l ~ r => mod(l, r) })
        | ("eql" ~> left ~ right ^^ { case l ~ r => eql(l, r) })

    def left: Parser[Reg] = Reg.values.map(v => v.toString ~> success(v)).reduce(_ | _)

    def right: Parser[RightV] = left ^^ Left.apply | raw"-?\d+".r ^^ (Right apply _.toInt)


  def run(program: Program, input: List[Int]): Int =
    import Stmt.*

    def updReg(regs: Map[Reg, Int], reg: Reg, right: RightV, op: (Int, Int) => Int) =
      regs.updatedWith(reg)(v => Some(op(v.get, right.fold(regs(_), identity))))

    var regs = Reg.values.map(reg => reg -> 0).toMap
    regs = program
      .foldLeft((regs, input)) {
        case ((regs, i :: input), inp(reg)) => (regs.updated(reg, i), input)
        case ((regs, input), add(reg, right)) => (updReg(regs, reg, right, (_ + _)), input)
        case ((regs, input), mul(reg, right)) => (updReg(regs, reg, right, (_ * _)), input)
        case ((regs, input), div(reg, right)) => (updReg(regs, reg, right, (_ / _)), input)
        case ((regs, input), mod(reg, right)) => (updReg(regs, reg, right, (_ % _)), input)
        case ((regs, input), eql(reg, right)) => (updReg(regs, reg, right, ((x, y) => if x == y then 1 else 0)), input)
      }
      ._1
    regs(Reg.z)


def tryThis(w: Int, z: Int, divZ: Int /*1 | 26*/ , addX: Int, addY: Int): Int =
  val x = if z % 26 + addX == w then 0 else 1
  (z / divZ) * (x * 25 + 1) + (w + addY) * x


val DataDivZAddXAddY = Vector(
  (1, 10, 2),
  (1, 15, 16),
  (1, 14, 9),
  (1, 15, 0),
  (26, -8, 1),
  (1, 10, 12),
  (26, -16, 6),
  (26, -4, 6),
  (1, 11, 3),
  (26, -3, 5),
  (1, 12, 9),
  (26, -7, 3),
  (26, -15, 2),
  (26, -7, 3),
)


def theProg(input: Vector[Int]): Int =
  (DataDivZAddXAddY zip input)
    .foldLeft(0) {
      case (z, ((divZ, addX, addY), w)) =>
        tryThis(w, z, divZ, addX, addY)
    }


@main def day24(): Unit =
  val input = Source
    .fromURL(getClass.getResource("/24.txt"))
    .getLines()

  val program: Program =
    input
      .map(Program.Parser.parseAll(Program.Parser.stmt, _).get)
      .toVector

  var failed = false
  for _ <- 1 to 100000 do
    val input = Vector.fill(15)(Random.between(1, 10))
    if theProg(input) != Program.run(program, input.toList) then
      println(s"${theProg(input)} != ${Program.run(program, input.toList)} for ${input}")
      failed = true
  println(s"Tests: ${if failed then "Failed" else "Success"}")

  val cache = mutable.Map.empty[(Int, Int), Seq[Vector[Int]]]

  def runMultiple(z: Int, dIndex: Int): Seq[Vector[Int]] =
    val cacheKey = (z, dIndex)

    for result <- cache.get(cacheKey)
      do return result

    if dIndex == 14 then
      return if z == 0 then Seq(Vector.empty) else Seq.empty

    val result =
      val (dz, ax, ay) = DataDivZAddXAddY(dIndex)

      for w <- 1 to 9
          nz = tryThis(w, z, dz, ax, ay)
          tail <- runMultiple(nz, dIndex + 1)
      yield w +: tail

    cache(cacheKey) = result
    result

  val rm = runMultiple(0, 0).map(_.mkString.toLong)

  println(rm.size)
  println(cache.size)
  println(rm.min)
  println(rm.max)
