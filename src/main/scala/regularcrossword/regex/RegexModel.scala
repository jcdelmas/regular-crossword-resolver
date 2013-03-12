package regularcrossword.regex


sealed abstract class Box

case class Regex(suites: Suite*) extends Box

case class Suite(selectors: Selector*)

case class Selector(box: Box, quantifier: Quantifier)

sealed abstract class Quantifier
case object One extends Quantifier
case object ZeroOrOne extends Quantifier
case object OneOrMore extends Quantifier
case object ZeroOrMore extends Quantifier

sealed abstract class CharMatcher extends Box

case object Wildcard extends CharMatcher
case class ExactMatch(c: Char) extends CharMatcher
case class Inclusion(chars: Set[Char]) extends CharMatcher
case class Exclusion(chars: Set[Char]) extends CharMatcher

case class GroupMatcher(n: Int) extends Box