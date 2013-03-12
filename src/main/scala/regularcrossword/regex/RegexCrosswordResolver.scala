package regularcrossword.regex

object RegexCrosswordResolver {
  
  import RegexGen._
  
  class Crossword(
      val size: Int,
      val xConstraints: Seq[Set[Seq[Constraint]]], 
      val yConstraints: Seq[Set[Seq[Constraint]]], 
      val zConstraints: Seq[Set[Seq[Constraint]]],  
      val dirties: Set[(Int, Int)],
      val toResolve: Map[(Int, Int), Set[Char]],
      val resolved: Map[(Int, Int), Char]
  ) {
    val radius = size / 2
    
    def resolve(): Iterable[Map[(Int, Int), Char]] = {
      if (xConstraints.isEmpty || yConstraints.isEmpty || zConstraints.isEmpty) {
        Nil
      } else if (toResolve.isEmpty) {
        List(resolved)
      } else if (dirties.isEmpty) {
        val ((x, y), values) = toResolve.minBy(_._2.size)
        values.view.flatMap {
          c => resolveWithValues(x, y, Set(c), dirties)
        }
      } else {
        val (x, y) = dirties.head
        val z = getZ(x, y)
        val xValues = allowedValues(xConstraints, x, y)
        val yValues = allowedValues(yConstraints, y, z)
        val zValues = allowedValues(zConstraints, z, x)
        val values = xValues intersect yValues intersect zValues
        if (3 * values.size < xValues.size + yValues.size + zValues.size) {
          resolveWithValues(x, y, values, dirties.tail)
        } else {
          new Crossword(size, 
            xConstraints, 
            yConstraints, 
            zConstraints, 
            dirties.tail, 
            toResolve, 
            resolved).resolve() 
        }
      }
    }
    
    def resolveWithValues(x: Int, y: Int, values: Set[Char], fromDirties: Set[(Int, Int)]) = {
      if (values.isEmpty) {
        List()
      } else {
        val z = getZ(x, y)
        val newXConstraints = updateAllowedValues(xConstraints, x, y, values)
        val newYConstraints = updateAllowedValues(yConstraints, y, z, values)
        val newZConstraints = updateAllowedValues(zConstraints, z, x, values)
          
        val newDirties = fromDirties ++ (
            newDirtyCases(xConstraints, newXConstraints, x, y) ++
            newDirtyCases(yConstraints, newYConstraints, y, z).map {
              case (y, z) => (getZ(y, z), y)
            } ++
            newDirtyCases(zConstraints, newZConstraints, z, x).map {
              case (z, x) => (x, getZ(z, x))
            }).filterNot(resolved.contains(_))
          
        val (newToResolve, newResolved) = if (values.size == 1) 
            (toResolve - ((x, y)), resolved.updated((x, y), values.head)) 
          else (toResolve.updated((x, y), values), resolved)
            
        new Crossword(size, 
            newXConstraints, 
            newYConstraints, 
            newZConstraints, 
            newDirties, 
            newToResolve, 
            newResolved).resolve() 
      }
    }
    
    def allowedValues(constraints: Seq[Set[Seq[Constraint]]], x: Int, y: Int) = {
      val i = index(x, y)
      constraints(x).foldLeft(Set[Char]()) { 
        (acc, solution) => acc ++ getValues(solution, i)
      }
    }
    
    def updateAllowedValues(constraints: Seq[Set[Seq[Constraint]]], x: Int, y: Int, values: Set[Char]) = {
      val i = index(x, y)
      val newConstraints = constraints(x).flatMap { 
        solution => updateValues(solution, i, values)
      }
      constraints.updated(x, newConstraints)
    }
    
    def getValues(solution: Seq[Constraint], i: Int): Set[Char] = solution(i) match {
      case Chars(chars) => chars
      case Ref(j)       => getValues(solution, j)
    }
    
    def updateValues(solution: Seq[Constraint], i: Int, values: Set[Char]): Option[Seq[Constraint]] = {
      solution(i) match {
        case Chars(oldValues) => {
          val newValues = values intersect oldValues
          if (newValues.isEmpty) None
          else if (newValues.size == oldValues.size) Some(solution)
          else Some(solution.updated(i, Chars(newValues)))
        }
        case Ref(j)           => updateValues(solution, j, values)
      }
    }
    
    def newDirtyCases(
        oldConstraints: Seq[Set[Seq[Constraint]]], 
        newConstraints: Seq[Set[Seq[Constraint]]],
        x: Int, y: Int) = {
      if (newConstraints(x) != oldConstraints(x)) {
        indexes(x, radius).filter(_ != y).map((x, _))
      } else {
        Set()
      }
    }
    
    def getZ(x: Int, y: Int) = 3 * radius - x - y
    
    def index(x: Int, y: Int) = if (x < radius) x + y - radius else y
  }
  
  def resolve(xs: Seq[String], ys: Seq[String], zs: Seq[String]): List[List[Char]] = {
    val xConstraints = computeConstraints(xs)
    val yConstraints = computeConstraints(ys)
    val zConstraints = computeConstraints(zs)
    
    val size = xs.length
    
    val dirties = (for {
      x <- (0 to size - 1)
      y <- indexes(x, size / 2)
    } yield (x, y)).toSet
    val toResolve = dirties.map((_, allValues)).toMap
    
    val solution = new Crossword(
        size, 
        xConstraints, 
        yConstraints, 
        zConstraints, 
        dirties, 
        toResolve, 
        Map()).resolve().head
        
    (0 to size - 1).map {
      x => indexes(x, size / 2).map(y => solution((x, y))).toList
    }.toList
  }
  
  def computeConstraints(regexs: Seq[String]) = {
    val radius = regexs.length / 2
    regexs.zipWithIndex.map {
      case (regex, i) => computeRegex(regex, regexs.length - Math.abs(radius - i))
    }
  }
  
  def computeRegex(regex: String, length: Int) = {
    RegexGen.compute(RegexParser.parse(regex), length)
  }
  
  def indexes(x: Int, radius: Int) = if (x < radius) {
    (radius - x to 2 * radius)
  } else {
    (0 to 3 * radius - x)
  }
  
  val allValues = ('A' to 'Z').toSet
}