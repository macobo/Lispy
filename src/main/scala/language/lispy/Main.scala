package language.lispy

import java.net.URL
import scala.util.parsing.combinator._
import scala.io.Source
import scala.util.Random
import scala.math

object Implicits {
    implicit def num2double(number: NUMBER) = number.value
    implicit def double2num(x: Double) = NUMBER(x)
    implicit def int2num(x: Int) = NUMBER(x)

    implicit def BOOL2bool(value: BOOLEAN) = value.value
    implicit def bool2BOOL = BOOLEAN(_)

    implicit def str2STR(s: String) = STRING(s)
    implicit def STR2str(str: STRING) = str.value
    //implicit def lst2LIST = LIST(_)
    //implicit def LIST2lst(lst: LIST) = lst.value
}

object Main {
    import LispyParser._
    import Function._
    import Implicits._

    val env = Env(NilEnv)

    def evaluate(text: String, env: Enviroment): Phrase =
        parse(text)(expr).eval(env)

    def evaluate(path: URL, env: Enviroment) = {
        val fileContents = Source.fromURL(path).getLines.mkString("\n")
        for (expr <- parse(fileContents)(program))
            expr.eval(env)
    }

    def register(env: Enviroment, name: String, what: Phrase) =
        env.assign(SYMBOL(name), what)

    def register(env: Enviroment, name: String, what: String) = {
        val fun = parseAll(expr, what).get.eval(env)
        env.assign(SYMBOL(name), fun)
    }

    def setupDefaults(env: Enviroment) = {
        register(env, "define", Define)
        register(env, "lambda", Lambda)
        register(env, "if", IfThenElse)
        register(env, "begin", Begin)
        register(env, "set!", SetF)
        register(env, "cond", Cond)
        register(env, "catch", Catch)

        register(env, "and", twoArgFunction[BOOLEAN](_ && _))
        register(env, "or", twoArgFunction[BOOLEAN](_ || _))
        register(env, "not", oneArgFunction[BOOLEAN](!_))

        register(env, "list?", oneArgFunction[Expression](x => x.isInstanceOf[LIST]))
        register(env, "null?", oneArgFunction[LIST](x => x.value.isEmpty))
        register(env, "length", Function { (args, env) =>
            evalEach(args, env) match {
                case LIST(s) :: Nil => s.length
                case STRING(s) :: Nil => s.length
                case args => throw TypeError("Expected (length lst), got args: " + args)
            }
        })
        register(env, "head", oneArgFunction[LIST](x => x.value.head))
        register(env, "tail", oneArgFunction[LIST](x => LIST(x.value.tail)))
        register(env, "cons", twoArgFunction(
            (head: Expression, lst: LIST) => LIST(head :: lst.value)))

        register(env, ">", twoArgFunction[NUMBER](_ > _))
        register(env, "=", twoArgFunction[Expression](_ == _))

        register(env, "+", twoArgFunction[NUMBER](_ + _))
        register(env, "*", twoArgFunction[NUMBER](_ * _))
        register(env, "/", twoArgFunction[NUMBER](_ / _))
        register(env, "-", "(lambda (x y) (+ x (* y -1)))")
        register(env, "floor", oneArgFunction[NUMBER](math.floor(_)))

        register(env, "addstr", twoArgFunction(
            (a: STRING, b: STRING) => a.value + b.value))
        register(env, "substr", Function { (args, env) =>
            evalEach(args, env) match {
                case STRING(s) :: NUMBER(frm) :: NUMBER(to) :: Nil =>
                    s.substring(frm.intValue(), to.intValue())
                case _ => throw TypeError("Expected (substr str from to), got args: " + args)
            }
        })
        register(env, "print", oneArgFunction[Expression](expr => {
            print(expr); LIST(Nil)
        }))
        register(env, "println", oneArgFunction[Expression](expr => {
            println(expr); LIST(Nil)
        }))
        register(env, "eval", oneArgFunction((s: STRING) =>
            parse(s.value)(program) map { _.eval(env) } last))
        register(env, "parse", oneArgFunction((s: STRING) =>
            try { parse(s.value)(program); BOOLEAN(true) }
            catch { case e => BOOLEAN(false) }))
        register(env, "random", Function { (args, env) =>
            if (args.isEmpty) Random.nextDouble()
            else throw TypeError("Expected (random), got args: " + args)
        })
        register(env, "readline", Function { (args, env) =>
            if (args.isEmpty) Console.readLine()
            else throw TypeError("Expected (readline), got args: " + args)
        })
        val exit = Function { (args, env) => sys.exit(); LIST(Nil); }
        register(env, "quit", exit)
        register(env, "exit", exit)
        evaluate(getClass.getResource("core.scm"), env)
        //register(env, "*globals*", Function { (args, env) => LIST(env.keys) })
    }

    def repl =
        evaluate(getClass.getResource("repl.scm"), env)

    def main(args: Array[String]): Unit = {
        setupDefaults(env)
        repl
    }
}