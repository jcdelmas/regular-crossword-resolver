package regularcrossword.regex

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class RegexGenSpec extends Specification {
  
  import RegexGen._

  "Simple generation" in {
    compute("A*", 3) must_== Set(
      Vector(Chars(Set('A')), Chars(Set('A')), Chars(Set('A')))  
    )
    compute("(A|BCD?)+", 4) must_== Set(
        Vector(Chars(Set('A')), Chars(Set('A')), Chars(Set('A')), Chars(Set('A'))),
        Vector(Chars(Set('A')), Chars(Set('A')), Chars(Set('B')), Chars(Set('C'))),
        Vector(Chars(Set('A')), Chars(Set('B')), Chars(Set('C')), Chars(Set('D'))),
        Vector(Chars(Set('A')), Chars(Set('B')), Chars(Set('C')), Chars(Set('A'))),
        Vector(Chars(Set('B')), Chars(Set('C')), Chars(Set('D')), Chars(Set('A'))),
        Vector(Chars(Set('B')), Chars(Set('C')), Chars(Set('A')), Chars(Set('A'))),
        Vector(Chars(Set('B')), Chars(Set('C')), Chars(Set('B')), Chars(Set('C')))
    )
    compute("A[AB]*D?", 4) must_== Set(
        Vector(Chars(Set('A')), Chars(Set('A', 'B')), Chars(Set('A', 'B')), Chars(Set('A', 'B'))),
        Vector(Chars(Set('A')), Chars(Set('A', 'B')), Chars(Set('A', 'B')), Chars(Set('D')))
    )
    compute("A[^YZ]", 2) must_== Set(Vector(Chars(Set('A')), Chars(('A' to 'X').toSet)))
    compute("(.*)", 1) must_== Set(Vector(Chars(('A' to 'Z').toSet)))
  }
  
  "Generation with group matching" in {
    compute("([AB]*)\\1", 4) must_== Set(
        Vector(Chars(Set('A', 'B')), Chars(Set('A', 'B')), Ref(0), Ref(1))
    )
  }
  
  def compute(regex: String, length: Int) = RegexGen.compute(RegexParser.parse(regex), length)
}