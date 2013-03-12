package regularcrossword.regex

object RegexGen {
  
  abstract class Constraint
  case class Chars(chars: Set[Char]) extends Constraint
  case class Ref(index: Int) extends Constraint
  
  def main(args: Array[String]) {
    compute(RegexParser.parse(".*(.)(.)(.)(.)\\4\\3\\2\\1.*"), 12)
  }
  
  class Groups(stack: List[(Int, Int)] = Nil, next: Int = 1, val matched: Map[Int, (Int, Int)] = Map()) {
    
    def startGroup(start: Int) = new Groups((next, start) :: stack, next + 1, matched)
    
    def endGroup(size: Int) = {
      val (n, start) = stack.head
      val newMatched = matched + ((n, (start, size)))
      new Groups(stack.tail, next, newMatched)
    }
  }
  
  class Context(
      val acc: List[Constraint], 
      val pos: Int,
      val min: Int, 
      val max: Int, 
      val groups: Groups,
      parent: Option[Context] = None) {
    
    def append(chunk: List[Constraint]) = {
      new Context(acc ::: chunk, pos + chunk.length, min - chunk.length, max - chunk.length, groups, parent)
    }
    
    def startGroup(newMin: Int) = new Context(Nil, pos, newMin, max, groups.startGroup(pos), Some(this))
    
    def endGroup = parent match {
      case Some(parent) => parent.setGroups(groups.endGroup(acc.length)).append(acc)
      case None         => this
    }
    
    private def setGroups(newGroups: Groups): Context = new Context(acc, pos, min, max, newGroups, parent)
  }
  
  val wildcardSet = ('A' to 'Z').toSet

  def compute(regex: Regex, length: Int): Set[Seq[Constraint]] = {
    computeRegex(regex, new Context(Nil, 0, length, length, new Groups())).map(_.acc.toIndexedSeq) 
  }
  
  def computeRegex(regex: Regex, ctx: Context) = regex.suites.flatMap {
    suite: Suite => computeSelectors(suite.selectors.toList, ctx)
  }.toSet
  
  def computeSelectors(selectors: List[Selector], ctx: Context): Set[Context] = selectors match {
    case head :: tail => computeSelector(head, ctx).flatMap { newCtx => 
      computeSelectors(tail, newCtx)
    }
    case Nil => if (ctx.min <= 0) Set(ctx) else Set()
  }
  
  def computeSelector(selector: Selector, ctx: Context): Set[Context] = 
    computeBoxWithQuantifier(selector.box, selector.quantifier, ctx)
  
  def computeBoxWithQuantifier(box: Box, quantifier: Quantifier, ctx: Context): Set[Context] = quantifier match {
    case One => computeBox(box, ctx, false)
    case ZeroOrOne => computeBox(box, ctx, true) + ctx
    case ZeroOrMore => computeBoxRec(box, ctx) + ctx
    case OneOrMore => computeBoxWithQuantifier(box, One, ctx).flatMap {
      newCtx: Context => computeBoxWithQuantifier(box, ZeroOrMore, newCtx)
    }
  }
  
  def computeBox(box: Box, ctx: Context, notEmpty: Boolean): Set[Context] = box match {
    case m: CharMatcher => 
      if (ctx.max > 0) {
        val matchedChars = computeCharMatcher(m, ctx)
        Set(ctx.append(List(matchedChars)))
      } else Set()
    case r: Regex       => {
      val subRegexMin = if (notEmpty) 1 else 0
      computeRegex(r, ctx.startGroup(subRegexMin)).map(_.endGroup)
    }
    case GroupMatcher(n) => {
      val (start, size) = ctx.groups.matched(n)
      if (size <= ctx.max) {
        val matched = (start to (start + size - 1)).map(Ref(_)).toList
        Set(ctx.append(matched))
      } else Set()
    }
  }
  
  def computeCharMatcher(m: CharMatcher, ctx: Context) = m match {
    case ExactMatch(c) => Chars(Set(c))
    case Inclusion(cs) => Chars(cs.toSet)
    case Exclusion(cs) => Chars(wildcardSet -- cs.toSet)
    case Wildcard      => Chars(wildcardSet)
  }
  
  def computeBoxRec(box: Box, ctx: Context): Set[Context] = ctx.max match {
    case n if n > 0 => computeBox(box, ctx, true).flatMap {
      newCtx: Context => computeBoxRec(box, newCtx) + newCtx
    }
    case _          => Set(ctx)
  }
}

