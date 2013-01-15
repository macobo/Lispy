package language.lispy

import Implicits._

sealed trait Phrase

sealed trait Expression extends Phrase {
    val value: Any
    def eval(env: Enviroment): Phrase
}

case class SYMBOL(value: String) extends Expression {
    def eval(env: Enviroment) = env.find(this)(this)
    override def toString = value
}

case class NUMBER(value: Double) extends Expression {
    def eval(env: Enviroment) = this
    override def toString = value.toString
}
case class BOOLEAN(value: Boolean) extends Expression {
    def eval(env: Enviroment) = this
    override def toString = if (value) "#t" else "#f"
}
case class STRING(value: String) extends Expression {
    def eval(env: Enviroment) = this
    override def toString = value
}

case class LIST(value: List[Phrase]) extends Expression {
    def eval(env: Enviroment) = {
        value match {
            case (head: SYMBOL) :: (rest: List[Expression]) => {
                val func = env.find(head)(head)
                func match {
                    case fn: Function => fn.call(rest, env)
                    case _ => throw TypeError(func + " is not callable.")
                }
            }
            case (head: Function) :: (rest: List[Expression]) =>
                head.call(rest, env)
            case (head: LIST) :: (rest: List[Expression]) =>
                LIST(head.eval(env) :: rest).eval(env)
            case Nil => this
            case _ => {
                throw new TypeError("Cannot call procedure with " + value.head + " (isn't a symbol), params: " + value)
            }
        }
    }
    override def toString = "(" + value.mkString(" ") + ")"
}

case class QUOTE(value: Expression) extends Expression {
    def eval(env: Enviroment) = value match {
        case UNQUOTE(expr) => expr.eval(env)
        case LIST(values) => LIST(values map {
            _ match {
                case UNQUOTE(expr) => expr.eval(env)
                case smth => smth
            }
        })
        case smth => smth
    }
    override def toString = "'" + value
}

case class UNQUOTE(value: Expression) extends Expression {
    def eval(env: Enviroment) = throw TypeError("naked unquote!")
}

case class Function(val call: (List[Expression], Enviroment) => Phrase) extends Phrase

object Function {
    def type_required(condition: Boolean, msg: String) =
        if (!condition) throw new TypeError(msg)
    def syntax_required(condition: Boolean, msg: String) =
        if (!condition) throw new SyntaxError(msg)

    def evalEach(args: List[Expression], env: Enviroment) =
        args map { _.eval(env) }

    def oneArgFunction[A <: Expression](fn: (A => Phrase)) = Function {
        (args, env) =>
            {
                evalEach(args, env) match {
                    case (a: A) :: Nil => fn(a)
                    case args => throw TypeError("Expected one-arg fn, got " + args)
                }
            }
    }
    def twoArgFunction[A <: Expression](fn: ((A, A) => Phrase)) = Function {
        (args, env) =>
            {
                evalEach(args, env) match {
                    case (a: A) :: (b: A) :: Nil => fn(a, b)
                    case args => throw TypeError("Expected two-arg fn, got " + args)
                }
            }
    }

    // SPECIAL FORMS:
    val Define = Function((args, env) => args match {
        case (sym: SYMBOL) :: expr :: Nil => env.assign(sym, expr.eval(env))
        case _ => throw SyntaxError("Expected (define what expr), args: " + args)
    })
    val Lambda = Function((args, _) => args match {
        case (head: SYMBOL) :: body :: Nil => Function({
            (args, env) => body.eval(env.child(head, evalEach(args, env)))
        })
        case LIST(head: List[Expression]) :: body :: Nil => Function {
            (args, env) => body.eval(env.child(head, evalEach(args, env)))
        }
        case _ => throw SyntaxError("Expected (define symbol expr), params were " + args)
    })
    val IfThenElse: Function = Function { (args, env) =>
        args match {
            case condition :: truebranch :: Nil =>
                IfThenElse.call(List(condition, truebranch, LIST(Nil)), env)
            case condition :: truebranch :: falsebranch :: Nil =>
                condition.eval(env) match {
                    case BOOLEAN(true) => truebranch.eval(env)
                    case BOOLEAN(false) => falsebranch.eval(env)
                    case cond => throw TypeError("Condition should expand to boolean")
                }
            case _ => throw SyntaxError("Expected (if cond truebranch falsebranch)")
        }
    }
    val Begin = Function { (args, env) => args.map { _.eval(env) }.last }
    val SetF = Function {
        (args, env) =>
            args match {
                case (sym: SYMBOL) :: expr :: Nil => {
                    env.find(sym).assign(sym, expr.eval(env))
                    sym
                }
                case _ => throw SyntaxError("Expected (set! what expr), args: " + args)
            }
    }
    val Cond: Function = Function { (clauses, env) =>
        clauses match {
            case LIST((SYMBOL("else") :: (body: Expression) :: Nil)) :: Nil =>
                body.eval(env)
            case LIST((cond: Expression) :: (body: Expression) :: Nil) :: rest =>
                cond.eval(env) match {
                    case BOOLEAN(true) => body.eval(env)
                    case BOOLEAN(false) => Cond.call(rest, env)
                    case _ => throw SyntaxError("Expected (cond (condition body)..)")
                }
            case Nil => LIST(Nil)
            case _ => throw SyntaxError("Expected (cond (condition body)..)")
        }
    }
    // (catch body)
    // If evaluating body throws an exception, returns the error message
    // If not, returns the result of evaluating body
    // (catch body catch-part) 
    // If evaluating body throws an exception, catch-part is evaluated.
    val Catch: Function = Function { (args, env) =>
        args match {
            case (body: Expression) :: Nil => {
                try { body.eval(env.child()) }
                catch { case e => STRING(e.getMessage()) }
            }
            case (body: Expression) :: (catch_part: Expression) :: Nil => {
                try { body.eval(env.child()) }
                catch { case e => catch_part.eval(env) }
            }
            case _ => throw SyntaxError("Expected (catch body catch-part), got args: " + args)
        }

    }
}
