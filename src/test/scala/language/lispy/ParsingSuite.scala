package language.lispy

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator._

@RunWith(classOf[JUnitRunner])
class ParsingSuite extends FunSuite {
    import LispyParser._

    val trickySymbol = "a-Valid-symbol12?!*"

    // Ordered as test, expected, comment
    val listTests = List(
        ("()", LIST(List()), "Empty list"),
        ("(1)", LIST(List(NUMBER(1))), "List containing a single number"),
        ("(1 2 abcd)", LIST(List(NUMBER(1), NUMBER(2), SYMBOL("abcd"))), "1,2,3"),
        ("(() (1 abc #f))", LIST(List(LIST(List()), LIST(List(NUMBER(1), SYMBOL("abc"), BOOLEAN(false))))), "Nesting"))

    def testParsing[T, E](parser: Parser[T], text: String, expected: E) = {
        assert(parse(text)(parser) === expected)
    }

    test("Parsing symbols using symbol parser") {
        testParsing(symbol, trickySymbol, SYMBOL(trickySymbol.toLowerCase()))
    }

    test("Parsing symbols with expression parser") {
        testParsing(expr, trickySymbol, SYMBOL(trickySymbol.toLowerCase()))
    }

    test("Parsing numbers using number parser") {
        testParsing(number, "12345", NUMBER(12345))
        testParsing(number, "12345.12345", NUMBER(12345.12345))
    }

    test("Parsing numbers using expression parser") {
        testParsing(expr, "12345", NUMBER(12345))
        testParsing(expr, "12345.12345", NUMBER(12345.12345))
    }

    test("Parsing booleans using boolean parser") {
        testParsing(boolean, "#t", BOOLEAN(true))
        testParsing(boolean, "#f", BOOLEAN(false))
        testParsing(boolean, "#T", BOOLEAN(true))
        testParsing(boolean, "#F", BOOLEAN(false))
    }

    test("Parsing booleans using expression parser") {
        testParsing(boolean, "#t", BOOLEAN(true))
        testParsing(boolean, "#f", BOOLEAN(false))
        testParsing(boolean, "#T", BOOLEAN(true))
        testParsing(boolean, "#F", BOOLEAN(false))
    }

    test("Parsing simple lists using list parser") {
        listTests foreach (
            test => testParsing(list, test._1, test._2))
    }

    test("Parsing simple lists using expression parser") {
        listTests foreach (
            test => testParsing(expr, test._1, test._2))
    }

    test("Quoted list using expression parser") {
        listTests foreach (
            test => testParsing(expr, "'" + test._1, QUOTE(test._2)))
    }

    test("Quoted symbol, number") {
        testParsing(expr, "'a", QUOTE(SYMBOL("a")))
        testParsing(expr, "'12", QUOTE(NUMBER(12)))
    }

    test("Comments should be ignored until EOL") {
        testParsing(expr, "a;ignorethis", SYMBOL("a"))
        testParsing(expr, "12;", NUMBER(12))
    }

    test("Nested comments") {
        testParsing(expr, "a;b;c;d", SYMBOL("a"))
    }

    test("Comments should last only until EOL") {
        testParsing(program, "a; somecomment\nb;othercomment", List(SYMBOL("a"), SYMBOL("b")))
    }

    test("String parsing") {
        testParsing(expr, "\"\"", STRING(""))
        testParsing(expr, "\"hello world!\"", STRING("hello world!"))
    }
}