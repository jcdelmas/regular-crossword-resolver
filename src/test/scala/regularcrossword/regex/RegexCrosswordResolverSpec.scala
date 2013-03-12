package regularcrossword.regex

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class RegexCrosswordResolverSpec extends Specification {

  "Simple resolution" in {
    RegexCrosswordResolver.resolve(
        List("[AB]*", ".*", "C."), 
        List("[CD]+", "([BF]).\\1", "G[AC]"),
        List(".[BC]", ".I.", "[BD]*")) must_== List(
            List('B', 'A'),
            List('D', 'I', 'G'),
            List('C', 'B')
        )
  }
}