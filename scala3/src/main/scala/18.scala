import org.jgrapht.util.DoublyLinkedList

import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.control.Breaks.{break, breakable}
import scala.util.parsing.combinator.RegexParsers


enum Token:
  case Open, Close, Comma
  case Val(value: Int)


sealed trait TreeNumber:
  def magnitude: Int

object TreeNumber:
  final case class Val(value: Int) extends TreeNumber :
    def magnitude: Int = value

  final case class Pair(left: TreeNumber, right: TreeNumber) extends TreeNumber :
    def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude

  object Parser extends RegexParsers :
    def value: Parser[Val] = raw"\d+".r ^^ (Val apply _.toInt)

    def pair: Parser[Pair] = '[' ~> number ~ ',' ~ number <~ ']' ^^ {
      case left ~ _ ~ right => Pair(left, right)
    }

    def number: Parser[TreeNumber] = value | pair


final class Number private(protected val data: DoublyLinkedList[Token]):
  override def toString: String =
    forwardIterator()
      .map(_.getValue)
      .map {
        case Token.Open => "["
        case Token.Close => "]"
        case Token.Comma => ","
        case Token.Val(number) => number
      }
      .mkString

  def copy: Number = Number(this.toString)

  type Node = DoublyLinkedList.ListNode[Token]

  object TokenNode:
    def unapply(node: Node): Some[Token] = Some(node.getValue)

  def forwardIterator(node: Node = null): Iterator[Node] =
    val start = Option(node).getOrElse(data.getFirstNode)
    Iterator
      .iterate(start)(_.getNext)
      .take(data.size() - data.indexOfNode(start))

  def backwardIterator(node: Node = null): Iterator[Node] =
    val start = Option(node).getOrElse(data.getLastNode)
    Iterator
      .iterate(start)(_.getPrev)
      .take(1 + data.indexOfNode(start))

  def +=(other: Number): Unit =
    add(other)
    breakable {
      while true do
        while (explodeOnce()) do ()
        if !splitOnce() then
          break
    }

  def add(other: Number): Unit =
    data.prepend(Number.parseToList("["))
    data.append(Number.parseToList(","))
    data.append(other.data)
    data.append(Number.parseToList("]"))

  def splitOnce(): Boolean =
    val big =
      forwardIterator()
        .collectFirst {
          case node@TokenNode(Token.Val(value)) if value > 9 => (node, value)
        }
    big match {
      case None =>
        false

      case Some((node, value)) =>
        val leftValue = value / 2
        val rightValue = value - leftValue
        Number.parseTokens(s"[$leftValue,$rightValue]")
          .foreach(data.addElementBeforeNode(node, _))

        data.removeNode(node)

        true
    }

  def explodeOnce(): Boolean =
    val exlosion =
      forwardIterator()
        .scanLeft((0, null: Node)) { (acc, node) =>
          val delta = node.getValue match {
            case Token.Open => 1
            case Token.Close => -1
            case _ => 0
          }
          (acc._1 + delta, node)
        }
        .drop(1) // remove initial (null, 0)
        .sliding(3)
        .map(_.unzip match {
          case (depths, nodes) => (depths.distinct, nodes)
        })
        .collectFirst {
          case (Seq(depth),
          Seq(leftNode@TokenNode(Token.Val(leftValue)), TokenNode(Token.Comma), rightNode@TokenNode(Token.Val(rightValue)))
            ) if depth > 4 => ((leftNode.getPrev, leftValue), (rightNode.getNext, rightValue))
        }

    exlosion match {
      case None =>
        false
      case Some(((leftNode, leftValue), (rightNode, rightValue))) =>
        for ((node, value) <- backwardIterator(leftNode).collectFirst { case node@TokenNode(Token.Val(value)) => (node, value) })
          do
            data.addElementBeforeNode(node, Token.Val(value + leftValue))
            data.removeNode(node)

        for ((node, value) <- forwardIterator(rightNode).collectFirst { case node@TokenNode(Token.Val(value)) => (node, value) })
          do
            data.addElementBeforeNode(node, Token.Val(value + rightValue))
            data.removeNode(node)

        data.addElementBeforeNode(leftNode, Token.Val(0))

        val index = data.indexOfNode(leftNode)
        for _ <- 1 to 5 do
          data.remove(index)

        true
    }

  def magnitude: Int =
    TreeNumber.Parser
      .parseAll(TreeNumber.Parser.number, this.toString)
      .get
      .magnitude

object Number:
  object Parser extends RegexParsers :
    def value: Parser[Token.Val] = raw"\d+".r ^^ (Token.Val apply _.toInt)

    def other(c: Char, t: Token): Parser[Token] = c ~> success(t)

    def token: Parser[Token] =
      value
        | other('[', Token.Open)
        | other(']', Token.Close)
        | other(',', Token.Comma)

    def tokens: Parser[Seq[Token]] = token.*

  def parseTokens(input: String): Seq[Token] =
    Parser
      .parseAll(Parser.tokens, input)
      .get

  def parseToList(input: String): DoublyLinkedList[Token] =
    val data = new DoublyLinkedList[Token]
    parseTokens(input).foreach(data.add)
    data

  def apply(input: String): Number =
    new Number(parseToList(input))


@main def day18(): Unit =
  val lines_1 =
    """
      |[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
      |[[[5,[2,8]],4],[5,[[9,9],0]]]
      |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
      |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
      |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
      |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
      |[[[[5,4],[7,7]],8],[[8,3],8]]
      |[[9,3],[[9,9],[6,[4,9]]]]
      |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
      |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
      |"""
      .strip()
      .stripMargin
      .linesIterator

  val lines = Source
    .fromURL(getClass.getResource("/18.txt"))
    .getLines()

  val numbers = lines.toVector

  val magnitudes =
    for i <- numbers.indices
        j <- numbers.indices
        if i != j
    yield
      val left = Number(numbers(i))
      left += Number(numbers(j))
      left.magnitude

  println(magnitudes.max)
