package language.lispy
import scala.collection.mutable.HashMap

abstract class Enviroment {
    def assign(symbol: SYMBOL, value: Phrase): SYMBOL
    def find(symbol: SYMBOL): Enviroment
    def apply(symbol: SYMBOL): Phrase

    def child(variables: SYMBOL, values: List[Phrase]): Enviroment
    def child(variables: List[Expression] = Nil, values: List[Phrase] = Nil): Enviroment
    def asString(level: Int): String
    def keys: List[SYMBOL]
}

case class Env(parent: Enviroment) extends Enviroment {
    val values = new HashMap[String, Phrase]()

    def assign(symbol: SYMBOL, value: Phrase): SYMBOL = {
        values.put(symbol.value, value)
        symbol
    }

    // Returns the innermost enviroment which contains the symbol
    def find(symbol: SYMBOL): Enviroment = {
        val key = symbol.value
        if (values contains key)
            this
        else
            parent.find(symbol)
    }

    def apply(symbol: SYMBOL): Phrase = {
        if (values contains symbol.value)
            values(symbol.value)
        else
            throw new RuntimeException("Error looking up symbol " + symbol)
    }

    def child(variables: SYMBOL, values: List[Phrase]): Enviroment = {
        val childEnv = Env(this)
        childEnv.assign(variables, LIST(values))
        childEnv
    }

    def child(variables: List[Expression], values: List[Phrase]): Enviroment = {
        if (variables.length != values.length)
            throw SyntaxError("The number of parameters and given arguments should match! (given: " + values)
        val childEnv = Env(this)
        variables.zip(values).foreach {
            pair => childEnv.assign(pair._1.asInstanceOf[SYMBOL], pair._2)
        }
        childEnv
    }

    def asString(level: Int) =
        "\nLevel " + level + "\n\t" + values.mkString("\n\t") + "\n--VVV--" + parent.asString(level + 1)

    def keys = values.keys.map { SYMBOL(_) }.toList
}

object NilEnv extends Enviroment {
    def assign(symbol: SYMBOL, value: Phrase) =
        throw new RuntimeException("Cannot assign to nil enviroment")
    def find(symbol: SYMBOL): Enviroment = throw new RuntimeException("Cannot find symbol " + symbol)
    def apply(symbol: SYMBOL): Phrase = throw new RuntimeException("Cannot find symbol " + symbol)

    def child(variables: SYMBOL, values: List[Phrase]): Enviroment =
        Env(this).child(variables, values)
    def child(variables: List[Expression], values: List[Phrase]): Enviroment =
        Env(this).child(variables, values)

    def asString(x: Int) = "N"
    def keys = Nil
}