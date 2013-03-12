package regularcrossword.regex

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class RegexParserSpec extends Specification {

  "Simple Regexp" in {
    RegexParser.parse("A") must_== Regex(Suite(Selector(ExactMatch('A'), One)))
  }
  
  "Quantifiers" in {
    RegexParser.parse("B+") must_== Regex(Suite(Selector(ExactMatch('B'), OneOrMore)))
    RegexParser.parse("C?D*") must_== 
      Regex(Suite(
          Selector(ExactMatch('C'), ZeroOrOne),
          Selector(ExactMatch('D'), ZeroOrMore)))
  }

  "Disjunction" in {
    RegexParser.parse("AB|CD") must_==
      Regex(
          Suite(Selector(ExactMatch('A'), One), Selector(ExactMatch('B'), One)),
          Suite(Selector(ExactMatch('C'), One), Selector(ExactMatch('D'), One)))
  }
  
  "Group" in {
    RegexParser.parse("A(B|C)D") must_== 
      Regex(Suite(
          Selector(ExactMatch('A'), One), 
          Selector(Regex(
              Suite(Selector(ExactMatch('B'), One)), 
              Suite(Selector(ExactMatch('C'), One))), One),
          Selector(ExactMatch('D'), One)))
  }
  
  "Char matchers" in {
    RegexParser.parse(".*") must_== Regex(Suite(Selector(Wildcard, ZeroOrMore)))
    RegexParser.parse("[AB]*") must_== Regex(Suite(Selector(Inclusion(Set('A', 'B')), ZeroOrMore)))
    RegexParser.parse("[^CD]E?") must_== 
      Regex(Suite(Selector(Exclusion(Set('C', 'D')), One), Selector(ExactMatch('E'), ZeroOrOne)))
  }
  
  "GroupMatcher" in {
    RegexParser.parse("(A*)(B+)\\2\\1") must_== 
      Regex(Suite(
          Selector(Regex(Suite(Selector(ExactMatch('A'), ZeroOrMore))), One),
          Selector(Regex(Suite(Selector(ExactMatch('B'), OneOrMore))), One),
          Selector(GroupMatcher(2), One),
          Selector(GroupMatcher(1), One)))
  }
}