package language.lispy
import scala.util.parsing.combinator.JavaTokenParsers

object LispyParser extends JavaTokenParsers {
    def expr: Parser[Expression] = boolean | number | symbol | quote | unquote | list | str
    def program = expr.*

    def boolean: Parser[BOOLEAN] = "#[tfTF]".r ^^ { s => BOOLEAN(s.toLowerCase() == "#t") }
    def symbol: Parser[SYMBOL] =
        "[0-9a-zA-Z-?!*+/><=]+".r ^^ { s => SYMBOL(s.toLowerCase) }

    def number: Parser[NUMBER] =
        (decimalNumber | floatingPointNumber | wholeNumber) ^^ { s => NUMBER(s.toDouble) }

    def str: Parser[STRING] = ("\"(.*?)\"".r) ^^ { s => STRING(s.substring(1, s.length() - 1)) }
    def list: Parser[LIST] = ("(" ~> expr.* <~ ")") ^^ { LIST(_) }
    def quote: Parser[QUOTE] = "'" ~> expr ^^ { QUOTE(_) }
    def unquote: Parser[UNQUOTE] = "," ~> expr ^^ { UNQUOTE(_) }

    val comment = """;.*""";
    def parse[A](text: String)(parser: Parser[A]): A = {
        parseAll(parser, text.replaceAll(comment, "")) match {
            case Success(result, _) => result
            case failure => throw new SyntaxError(failure.toString)
        }
    }
}

case class SyntaxError(msg: String) extends RuntimeException(msg)
case class TypeError(msg: String) extends RuntimeException(msg)