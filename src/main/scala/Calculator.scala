import scala.util.parsing.combinator.RegexParsers

object Calculator extends RegexParsers {

  val NUMBER = """\d+""".r

  def int: Parser[Int] = NUMBER ^^ { _.toInt }

  def plus: Parser[Int] = "+" ~ expr ^^ {
    case "+" ~ elt => elt
  }
  
  def minus: Parser[Int] = "-" ~ expr ^^ {
    case "-" ~ elt => -elt
  }
  
  def changer: Parser[Int] = plus | minus
  
  def expr: Parser[Int] = int ~ rep(changer) ^^ {
    case first ~ second => first + second.sum
  }

  def eval: Parser[Int] = expr

  def apply(input: String): Number = parseAll(eval, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}