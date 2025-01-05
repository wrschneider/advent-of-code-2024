import scala.annotation.tailrec

object p21 {

  val digits = Seq("789", "456", "123", " 0A")

  def allPathsBetween(keypad: Seq[String], start: (Int, Int), end: (Int, Int)): List[String] = {
    val dr = end._1 - start._1
    val dc = end._2 - start._2
    if (dr == 0 && dc == 0) {
      List("A")
    } else {
      val pathsWithUp = if (dr < 0 && start._1 >= 0 && start._1 < keypad.length && keypad(start._1 - 1)(start._2) != ' ') {
        allPathsBetween(keypad, (start._1 - 1, start._2), end).map("^" + _)
      } else Nil
      // copilot did these after first:
      val pathsWithDown = if (dr > 0 && start._1 >= 0 && start._1 < keypad.length && keypad(start._1 + 1)(start._2) != ' ') {
        allPathsBetween(keypad, (start._1 + 1, start._2), end).map("v" + _)
      } else Nil
      val pathsWithLeft = if (dc < 0 && start._2 >= 0 && start._2 < keypad.head.length && keypad(start._1)(start._2 - 1) != ' ') {
        allPathsBetween(keypad, (start._1, start._2 - 1), end).map("<" + _)
      } else Nil
      val pathsWithRight = if (dc > 0 && start._2 >= 0 && start._2 < keypad.head.length && keypad(start._1)(start._2 + 1) != ' ') {
        allPathsBetween(keypad, (start._1, start._2 + 1), end).map(">" + _)
      } else Nil
      pathsWithUp ::: pathsWithRight ::: pathsWithLeft ::: pathsWithDown
    }
  }

  def keypadPresses(keypad: Seq[String], code: String): Seq[String] = {
    val startPos = keypad.indices.flatMap { r =>
      val c = keypad(r).indexOf("A")
      if (c >= 0) Some((r, c)) else None
    }.head

    code.foldLeft((List.empty[String], startPos)) { (acc, ch) =>
      val (pathsSoFar, currPos) = acc
      // find next char
      val nextPos = keypad.indices.flatMap { r =>
        val c = keypad(r).indexOf(ch)
        if (c >= 0) Some((r, c)) else None
      }.head

      val pathsToNext = allPathsBetween(keypad, currPos, nextPos)
      // cross product between paths so far and next path
      val nextPaths = if (pathsSoFar.isEmpty) pathsToNext else pathsSoFar.flatMap { path =>
        pathsToNext.map(path + _)
      }
      (nextPaths, nextPos)
    }._1
  }

  // hard-wire expansion of sequences based on samples + futzing around to see what gives lower totals
  // # of legal moves on arrow keypad is limited enough to allow brute-force expansion.
  // When you account for (a) repeated keys being better than alternating and (b) some sequences being illegal,
  //  the only real decision point is moving to "v" arrow via v< or <v; and moving from v to A via >^ or ^>
  // other sequences like ^^<< only appear as sequences for initial keypad
  val expandOneLevel = Map(
    "" -> Seq(""),
    //"<v<" -> Seq("<v<", ">", "<", ">>^"),
    "<^" -> Seq("v<<", ">^", ">"),
    "<v" -> Seq("v<<", ">", "^>"),
    "<" -> Seq("v<<", ">>^"),
    "v" -> Seq("<v", "^>"),
    "^" -> Seq("<", ">"),
    ">" -> Seq("v", "^"),
    "v<<" -> Seq("<v", "<", "", ">>^"),
    ">>^" -> Seq("v", "", "<^", ">"),
    ">^" -> Seq("v", "<^", ">"),
    ">v" -> Seq("v", "<", "^>"),
    "^^>" -> Seq("<", "", "v>", "^"),
    "vvv" -> Seq("<v", "", "", "^>"),
    "^^^" -> Seq("<", "", "", ">"),
    "^<<" -> Seq("<", "v<", "", ">>^"),
    "vv" -> Seq("<v", "", "^>"),
    "^^" -> Seq("<", "", ">"),
    ">>" -> Seq("v", "", "^"),
    "<<" -> Seq("v<<", "", ">>^"),
    "^^<<" -> Seq("<", "", "v<", "", ">>^"),
    "<<^^" -> Seq("v<<", "", ">^", "", ">"),
    "<^^^" -> Seq("v<<", ">^", "", "", ">"),
    "^^^<" -> Seq("<", "", "", "v<", ">>^"),
    "v<" -> Seq("<v", "<", ">>^"),
    ">vv" -> Seq("v", "<", "", "^>"),
    ">>vv" -> Seq("v", "", "<", "", "^>"),
    ">vvv" -> Seq("v", "<", "", "", "^>"),
    "vvv>" -> Seq("<v", "", "", ">", "^"),
    "<<^" -> Seq("v<<", "", ">^", ">"),
    "^<<" -> Seq("<", "v<", "", ">>^"),
    ">>v" -> Seq("v", "", "<", "^>"),
    "vv>" -> Seq("<v", "", ">", "^"),
    "^>" -> Seq("<", "v>", "^"),
    "v>" -> Seq("<v", ">", "^"),
    "^<" -> Seq("<", "v<", ">>^"),
  )

  @tailrec
  def expandCounts(counts: Map[String, Long], i: Integer): Map[String, Long] = {
    if (i == 0) {
      counts
    } else {
      // expand one level
      val nextCounts = counts.foldLeft(Map.empty[String, Long]) { (nextMap, pair) =>
        val (code, count) = pair
        val nextCodes = expandOneLevel(code)
        nextCodes.foldLeft(nextMap) { (acc, nextCode) =>
          val nextCount = acc.getOrElse(nextCode, 0L) + count
          acc + (nextCode -> nextCount)
        }
      }
      expandCounts(nextCounts, i - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val codes2 = "029A\n980A\n179A\n456A\n379A".split("\n")
    val codes = "805A\n964A\n459A\n968A\n671A".split("\n")

    val finalOutput = codes.map { code =>
      println(code)
      val p1 = keypadPresses(digits, code)
      println(p1)

      val robotKeypads = 25 // part 1 = 2, part 2 = 25
      p1.flatMap(p => {
        val initialCounts = p.split("A").groupBy(identity).mapValues(_.length.toLong)
        // don't consider patterns like <^<^ because <<^^ or ^^<< must be shorter
        initialCounts.foreach(c => if (!expandOneLevel.contains(c._1)) println("Warning: " + c))

        if (initialCounts.forall(c => expandOneLevel.contains(c._1))) {
          val finalCounts = expandCounts(initialCounts, robotKeypads)
          Some(finalCounts.map(c => (c._1.length + 1) * c._2).sum)
        } else None
      }).min
    }
    val result = codes.zip(finalOutput).map { case (code, output) =>
      println(code, output)
      output * code.substring(0, 3).toInt
    }.sum
    println(result)
    //384886503529684 too high!
    //384041985863254 a little lower, still too high
    // 382248515937734 wrong answer
    // 361653034382136 wrong answer
    //349776057016716 wrong answer
    // 337244995283638 wrong --> included some illegal moves
    // 337744744231414 -> finally right answer!

    //153758411085208 too low - so believe we are on right order of magnitude
  }
}
