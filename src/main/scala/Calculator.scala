import scala.util.parsing.combinator.RegexParsers

object Calculator extends RegexParsers {

  val NUMBER = """\d+""".r

  def int: Parser[NumberNode] = NUMBER ^^ {
    case elt => NumberNode(elt.toInt)
  }

  def parens: Parser[Node] = "(" ~ expr ~ ")" ^^ {
    _._1._2
  }
  
  def multiplication: Parser[Node] = term ~ "*" ~ expr ^^ {
    x => OpNode("*", x._1._1, x._2)
  }
  
  def addsub: Parser[Node] = add | sub
  
  def add: Parser[Node] = term ~ "+" ~ expr ^^ {
    x => OpNode("+", x._1._1, x._2)
  }
  
  def sub: Parser[Node] = term ~ "-" ~ expr ^^ {
    x => OpNode("-", x._1._1, x._2)
  }
  
  def term: Parser[Node] = parens | int

  def expr: Parser[Node] = addsub | multiplication | term

  def eval: Parser[Int] = expr ^^ {
    _.eval
  }

  def apply(input: String): Number = parseAll(eval, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

trait Node {
  def eval: Int = this match {
    case NumberNode(contents) => contents
    case OpNode("*", left, right) => left.eval * right.eval
    case OpNode("+", left, right) => left.eval + right.eval
    case OpNode("-", left, right) => left.eval - right.eval
  }
}

case class NumberNode(contents: Int) extends Node

case class OpNode(contents: String, left: Node, right: Node) extends Node 
