package regularcrossword.regex

object RegexCrossword {

  def main(args: Array[String]) {
    resolve(
        List(
            ".*H.*H.*", 
            "(DI|NS|TH|OM)*", 
            "F.*[AO].*[AO].*", 
            "(O|RHH|MM)*", 
            ".*", 
            "C*MC(CCC|MM)*",
            "[^C]*[^R]*III.*",
            "(...?)\\1*",
            "([^X]|XCC)*",
            "(RR|HHH)*.?",
            "N.*X.X.X.*E",
            "R*D*M*",
            ".(C|HH)*"),
        List(
            ".*G.*V.*H.*",
            "[CR]*",
            ".*XEXM*",
            ".*DD.*CCM.*",
            ".*XHCR.*X.*",
            ".*(.)(.)(.)(.)\\4\\3\\2\\1.*",
            ".*(IN|SE|HI)",
            "[^C]*MMM[^C]*",
            ".*(.)C\\1X\\1.*",
            "[CEIMU]*OH[AEMOR]*",
            "(RX|[^R])*",
            "[^M]*M[^M]*",
            "(S|MM|HHH)*"),
        List(
            ".*SE.*UE.*",
            ".*LR.*RL.*",
            ".*OXR.*",
            "([^EMC]|EM)*",
            "(HHX|[^HX])*",
            ".*PRR.*DDC.*",
            ".*",
            "[AM]*CM(RC)*R?",
            "([^MC]|MM|CC)*",
            "(E|CR|MN)*",
            "P+(..)\\1.*",
            "[CHMNOR]*I[CHMNOR]*",
            "(ND|ET|IN)[^X]*")
    )
  }
  
  def resolve(xs: Seq[String], ys: Seq[String], zs: Seq[String]) {
    val start = System.currentTimeMillis()
    val result = RegexCrosswordResolver.resolve(xs, ys, zs)
    val end = System.currentTimeMillis()
    
    println("Resolved in " + (end - start) + "ms: ")
    var radius = xs.length / 2
    result.zipWithIndex.foreach {
      case (line, i) => {
        for (_ <- 1 to Math.abs(radius - i)) print(" ")
        println(line.mkString(" "))
      }
    }
  }
}