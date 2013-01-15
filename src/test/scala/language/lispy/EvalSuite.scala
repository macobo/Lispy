package language.lispy

import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.parsing.combinator._
import org.scalatest.BeforeAndAfter

@RunWith(classOf[JUnitRunner])
class EvalSuite extends FlatSpec with BeforeAndAfter {
    import LispyParser._
    import Function._
    import Implicits._
    implicit def double2num(number: Double) = NUMBER(number)

    var env: Enviroment = _
    def evaluate(text: String): Phrase = parseAll(expr, text).get.eval(env)

    before {
        env = Env(NilEnv)
        Main.setupDefaults(env)
    }

    "NUMBER.eval" should "return the number" in {
        assert(evaluate("5") === NUMBER(5))
    }

    "BOOLEAN.eval" should "return the correct boolean" in {
        assert(evaluate("#T") === BOOLEAN(true))
        assert(evaluate("#f") === BOOLEAN(false))
    }

    "LIST.eval" should "return () on ()" in {
        assert(evaluate("()") === LIST(List()))
    }

    it should "fail on (1 2 3)" in {
        intercept[RuntimeException] { evaluate("(1 2 3)") }
    }

    "QUOTE.eval" should "return the inner value unevaluated" in {
        assert(evaluate("'(1 2 a)") === LIST(List(NUMBER(1), NUMBER(2), SYMBOL("a"))))
        assert(evaluate("'a") === SYMBOL("a"))
    }

    it should "evaluate inner unquotes (requires num ops)" in {
        assert(evaluate("',(+ 1 2)") === NUMBER(3))
    }

    "naked unquote" should "throw an error" in {
        intercept[Exception] { evaluate(",3") }
    }

    "(define symbol expr)" should "not fail in valid case" in {
        evaluate("(define a 3)")
        evaluate("(define a '(1 2))")
    }

    it should "fail if no of arguments is not 2" in {
        intercept[SyntaxError] { evaluate("(define)") }
        intercept[SyntaxError] { evaluate("(define a)") }
        intercept[SyntaxError] { evaluate("(define a 3 4)") }
    }

    it should "fail if first argument is not a symbol" in {
        intercept[RuntimeException] { evaluate("(define 1 3)") }
        intercept[RuntimeException] { evaluate("(define () 3)") }
        intercept[RuntimeException] { evaluate("(define #t 3)") }
    }

    it should "return the symbol used" in {
        assert(evaluate("(define abc 6)") == SYMBOL("abc"))
    }

    it should "update the value of the variable" in {
        evaluate("(define a 5)")
        assert(evaluate("a") === NUMBER(5))
        evaluate("(define a '(1 (a)))")
        assert(evaluate("a") === LIST(List(NUMBER(1), LIST(List(SYMBOL("a"))))))
    }

    "(lambda (params..) expr)" should "should yield a function" in {
        assert(evaluate("(lambda () 3)").isInstanceOf[Function])
    }

    it should "evaluate to expr - literal expr" in {
        assert(evaluate("((lambda () 3))") === NUMBER(3))
    }

    it should "evaluate to expr - expr" in {
        assert(evaluate("((lambda () '(1 3)))") === LIST(List(NUMBER(1), NUMBER(3))))
    }

    it should "evaluate parameters" in {
        assert(evaluate("((lambda (x) x) 9)") === NUMBER(9))
    }

    it should "work nested" in {
        assert(evaluate("(((lambda () (lambda () 3))))") === NUMBER(3))
    }

    it should "throw a syntax error if params is not a list" in {
        intercept[SyntaxError] { evaluate("(lambda 3 4)") }
        intercept[SyntaxError] { evaluate("(lambda '(3 4) 4)") }
        intercept[SyntaxError] { evaluate("(lambda #t 4)") }
    }

    it should "work with define to form a function" in {
        evaluate("(define fn (lambda (x) x))")
        assert(evaluate("(fn 3)") === NUMBER(3))
    }

    it should "bind args to single symbol" in {
        assert(evaluate("((lambda x x) 3 4)") === LIST(List(3, 4)))
    }

    it should "work to form a cube function" in {
        evaluate("(define square (lambda (x y z) (* x (* y z))))")
        assert(evaluate("(square 2 3 4)") === NUMBER(24))
    }

    ">" should "work with two NUMBER-s" in {
        assert(evaluate("(> 3 4)") === BOOLEAN(false))
        assert(evaluate("(> 5 4)") === BOOLEAN(true))
        assert(evaluate("(> 4 4)") === BOOLEAN(false))
    }

    it should "throw an Error with non-numerics" in {
        intercept[ClassCastException] { evaluate("(> 'a 'b)") }
    }

    "+*/ functions" should "work on numbers" in {
        assert(evaluate("(+ 3 4)") === NUMBER(7))
        assert(evaluate("(* 3 4)") === NUMBER(12))
        assert(evaluate("(/ 3 4)") === NUMBER(0.75))
        assert(evaluate("(+ 1 (+ 2 3))") === NUMBER(6))
    }

    it should "work on nested arguments" in {
        assert(evaluate("(* 2 (+ 2 9))") === NUMBER(22))
        assert(evaluate("(+ (+ 2 9) (+ 2 9))") === NUMBER(22))
    }

    "- function" should "work on numbers" in {
        assert(evaluate("(- 4 5)") === NUMBER(-1))
        assert(evaluate("(- 4 3)") === NUMBER(1))
    }

    it should "work on nested arguments" in {
        assert(evaluate("(- (- 5 4) (- 5 9))") === NUMBER(5))
    }

    "(if condition truebranch falsebranch)" should "work - simple case" in {
        assert(evaluate("(if #t 3 4)") === NUMBER(3))
        assert(evaluate("(if #f 3 4)") === NUMBER(4))
    }

    it should "not evaluate the other branch" in {
        assert(evaluate("(if #t 3 (some-function-that-doesn't-exist '3))") === NUMBER(3))
        assert(evaluate("(if #f (some-function-that-doesn't-exist '3) 3)") === NUMBER(3))
    }

    "recursion" should "work in simple example" in {
        evaluate("(define recursive (lambda (x) (if (> x 0) (recursive 0) 3)))")
        assert(evaluate("(recursive 0)") === NUMBER(3))
        assert(evaluate("(recursive 9)") === NUMBER(3))
    }

    "basic list operations" should "work" in {
        assert(evaluate("(head '(1 2 3))") === NUMBER(1))
        intercept[Exception] { evaluate("(head '())") }
        assert(evaluate("(tail '(1 2 3))") === LIST(List(2, 3)))
        assert(evaluate("(tail '(1))") === LIST(Nil))
        assert(evaluate("(null? '(1))") === BOOLEAN(false))
        assert(evaluate("(null? '())") === BOOLEAN(true))
    }

    "map function" should "work" in {
        evaluate("(define square (lambda (x) (* x x)))")
        assert(evaluate("(map square '(1 2 3))") === LIST(List(1, 4, 9)))
        assert(evaluate("(map square '())") === LIST(Nil))
        intercept[Exception] { evaluate("(map square 'c)") }
        intercept[Exception] { evaluate("(map square)") }
    }

    "numeric functions" should "all work" in {
        assert(evaluate("(< 3 4)") === BOOLEAN(true))
        assert(evaluate("(< 5 4)") === BOOLEAN(false))
        assert(evaluate("(< 4 4)") === BOOLEAN(false))
        assert(evaluate("(<= 3 4)") === BOOLEAN(true))
        assert(evaluate("(<= 5 4)") === BOOLEAN(false))
        assert(evaluate("(<= 4 4)") === BOOLEAN(true))
        assert(evaluate("(>= 3 4)") === BOOLEAN(false))
        assert(evaluate("(>= 5 4)") === BOOLEAN(true))
        assert(evaluate("(>= 4 4)") === BOOLEAN(true))
        assert(evaluate("(abs -4)") === NUMBER(4))
        assert(evaluate("(abs 4)") === NUMBER(4))
    }

    "min function" should "return minimal" in {
        assert(evaluate("(min 3)") === NUMBER(3))
        assert(evaluate("(min 3 4)") === NUMBER(3))
        assert(evaluate("(min 3 5 -1 7)") === NUMBER(-1))
    }

    "string operations" should "work" in {
        evaluate("(define a \"some-string :)\")")
        evaluate("(define b \" other-stuff!\")")
        assert(evaluate("a") === STRING("some-string :)"))
        assert(evaluate("b") === STRING(" other-stuff!"))
        assert(evaluate("(addstr a b)") === STRING("some-string :) other-stuff!"))
        assert(evaluate("(length \"abcd\")") === NUMBER(4))
    }

    "cond" should "return first matching" in {
        assert(evaluate("(cond (#f 3) ((> 4 3) 7) (#t 5))") === NUMBER(7))
    }

    it should "only evaluate up to a match" in {
        assert(evaluate("(cond (#t 3) ((car '3) 7))") === NUMBER(3))
    }

    it should "match else and return Nil if no matches" in {
        assert(evaluate("(cond ((< 7 6) 9) (else 7))") === NUMBER(7))
        assert(evaluate("(cond ((< 7 6) 9) ((> 9 9) 10))") === LIST(Nil))
    }

    "*globals*" should "return a list of symbols containing current env. keys" in {
        val a = evaluate("(*globals*)").asInstanceOf[LIST]
        assert(a.value.contains(SYMBOL("cond")))
    }

    "type functions" should "work" in {
        assert(evaluate("(list? '(1 2 3))") === BOOLEAN(true))
        assert(evaluate("(list? 3)") === BOOLEAN(false))
    }
}