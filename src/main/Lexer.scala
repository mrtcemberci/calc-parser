package calc

import scala.annotation.tailrec

object Lexer:

    private val numRegex   = """^(\d+)""".r
    private val opRegex    = """^([\+\-\*/\(\)])""".r
    private val spaceRegex = """^(\s+)""".r

    def tokenize(input: String): List[Token] =
        @tailrec
        def loop(remaining: String, acc: List[Token]): List[Token] =
            if remaining.isEmpty then 
                acc.reverse
            else
                remaining match
                    case s if spaceRegex.findPrefixOf(s).isDefined =>
                        val matchStr = spaceRegex.findPrefixOf(s).get
                        loop(remaining.substring(matchStr.length), acc)
                    
                    case s if numRegex.findPrefixOf(s).isDefined =>
                        val matchStr = numRegex.findPrefixOf(s).get
                        loop(remaining.substring(matchStr.length), Num(matchStr.toInt) :: acc)
                    
                    case s if opRegex.findPrefixOf(s).isDefined =>
                        val matchStr = opRegex.findPrefixOf(s).get
                        val token = matchStr match
                            case "+" => Plus
                            case "-" => Minus
                            case "*" => Times
                            case "/" => Div
                            case "(" => LParen
                            case ")" => RParen
                        loop(remaining.substring(matchStr.length), token :: acc)

                    case _ => 
                        throw new Exception(s"Lexer Error: Unknown character at '${remaining.take(1)}'")

        loop(input, Nil)