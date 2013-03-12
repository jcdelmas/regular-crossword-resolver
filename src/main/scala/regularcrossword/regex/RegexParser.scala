package regularcrossword.regex

import scala.util.parsing.combinator.RegexParsers

object RegexParser extends RegexParsers {
  
  def box: Parser[Box] = charMatcher |
		  				 groupMatcher |
		  				 "(" ~> regex <~ ")"

  def regex: Parser[Regex] = rep1sep(suite, "|") ^^ { ss: List[Suite] => Regex(ss: _*) }
  
  def suite: Parser[Suite] = rep1(selector) ^^ { ss: List[Selector] => Suite(ss: _*) }
  
  def selector: Parser[Selector] = box ~ opt(quantifier) ^^ {
    case b ~ q => Selector(b, q.getOrElse(One))
  }
  
  def quantifier: Parser[Quantifier] = "+" ^^^ OneOrMore |
		  							   "*" ^^^ ZeroOrMore |
		  							   "?" ^^^ ZeroOrOne
  
  def charMatcher: Parser[CharMatcher] = "." ^^^ Wildcard | 
  										 exactMatch |
  										 inclusion |
  										 exclusion
  										 
  def exactMatch: Parser[ExactMatch] = """[A-Z]""".r ^^ {
    c: String => ExactMatch(c.charAt(0))
  }
  
  def inclusion: Parser[Inclusion] = "[" ~> """[A-Z]+""".r <~ "]" ^^ {
    cs: String => Inclusion(cs.toSet)
  }
  
  def exclusion: Parser[Exclusion] = "[^" ~> """[A-Z]+""".r <~ "]" ^^ {
    cs: String => Exclusion(cs.toSet)
  }
  
  def groupMatcher: Parser[GroupMatcher] = "\\" ~> """[1-9]+""".r ^^ {
    n: String => GroupMatcher(n.toInt)
  }
  
  def parse(input: String): Regex = parseAll(regex, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}
