import scala.io.Source
import scala.util.parsing.combinator.*

final case class Packet(version: Int, `type`: Int, content: Either[Long, Seq[Packet]]):
  def literal = content.left.toOption.get

  def packets = content.toOption.get

extension (data: String)
  def toBinaryInt: Int = Integer.parseInt(data, 2)
  def toBinaryLong: Long = java.lang.Long.parseLong(data, 2)

object PacketParser extends RegexParsers :
  def packetP: Parser[Packet] =
    versionP ~ typeP >> {
      case v ~ t =>
        if t == 4 then
          literalP ^^ { l => Packet(v, t, Left(l)) }
        else
          packetSeqP ^^ { ps => Packet(v, t, Right(ps)) }
    }

  def digitsP(lenght: Int): Parser[String] = repN(lenght, """\d""".r) ^^ (_.mkString)

  def binaryIntP(lenght: Int): Parser[Int] = digitsP(lenght) ^^ (_.mkString.toBinaryInt)

  def threeDigitsNumber: Parser[Int] = binaryIntP(3)

  def versionP: Parser[Int] = threeDigitsNumber

  def typeP: Parser[Int] = threeDigitsNumber

  def literalP: Parser[Long] = ('1' ~> digitsP(4)).* ~ ('0' ~> digitsP(4)) <~ """\d{0,3}?""".r ^^ {
    case xs ~ x => xs.mkString("", "", x).toBinaryLong
  }

  def packetSeqP: Parser[Seq[Packet]] =
    ('0' ~> binaryIntP(15) >> (repN(_, ".".r) ^^ (_.mkString) ^^ (parseAll(packetP.*, _).get)))
      | ('1' ~> binaryIntP(11) >> (repN(_, packetP)))

def parsePacket(hexadecimal: String): Packet =
  val data: String =
    hexadecimal.flatMap {
      c =>
        Integer
          .parseInt(c.toString, 16)
          .toBinaryString
          .reverse
          .padTo(4, '0')
          .reverse
    }

  val result = PacketParser.parse(PacketParser.packetP, data)
  result.get

def versions(packet: Packet): Seq[Int] =
  packet.version +: packet.content.toSeq.flatten.flatMap(versions)

def compute(packet: Packet): Long =
  lazy val computePackets = packet.packets.map(compute)
  packet.`type` match
    case 0 => computePackets.sum
    case 1 => computePackets.product
    case 2 => computePackets.min
    case 3 => computePackets.max
    case 4 => packet.literal
    case 5 =>
      val Seq(f, s) = computePackets
      if f > s then 1 else 0
    case 6 =>
      val Seq(f, s) = computePackets
      if f < s then 1 else 0
    case 7 =>
      val Seq(f, s) = computePackets
      if f == s then 1 else 0

@main def day16(): Unit =
  val input = Source
    .fromURL(getClass.getResource("/16.txt"))
    .getLines()
    .next()

  val packet = parsePacket(input)
  println(compute(packet)) // 660797830937
