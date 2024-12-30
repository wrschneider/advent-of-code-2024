object p21 {

  val digits = Seq("789", "456", "123", " 0A")
  val arrows = Seq(" ^A", "<v>")

  val arrowMap = Map(
    "AA" -> "A", // extra A by itself
    "<<" -> "A",
    ">>" -> "A",
    "vv" -> "A",
    "^^" -> "A",
    "<v" -> ">A",
    "<^" -> ">^A",
    // ">v" "<>" and "><" never appear except in initial
    ">v" -> "<A",
    ">^" -> "<^A",
    "v<" -> "<A",
    // v^, ^v never appear
    // ^<, ^> never appear except in paths that don't live past first expansion
    // never appears though "v>" -> Seq(">A"),
    "v>" -> ">A",
    "^<" -> "v<A",
    "^>" -> "v>A",
    "A^" -> "<A",
    "Av" -> "<vA",
    "A>" -> "vA",
    "A<" -> "<<vA",
    "^A" -> ">A",
    ">A" -> "^A",
    "<A" -> ">>^A",
    "vA" -> ">^A"
  )

  val arrowSeqMap: Map[String, Seq[String]] = arrowMap.map { case (k, v) => (k, v.sliding(2).toSeq) } + ("A" -> Seq("A"))
  for (elem <- arrowSeqMap) {
    println(elem)
  }

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

  def expand(code: String, i: Integer): String = {
    (1 to i).foldLeft(code) { (acc, x) =>
      val next = acc.sliding(2).foldLeft("A") { (newCode, pair) =>
        val next = arrowMap(pair)
        newCode + next
      }
      println(x, next.length)
      //next.groupBy(identity).mapValues(_.size).foreach(println)
      next
    }
  }

  def expandCounts(code: String, i: Integer): Map[String, Int] = {
    val initialSequenceCounts: Map[String, Int] = code.sliding(2)
      .flatMap(s => arrowSeqMap(s)).toSeq.groupBy(identity).mapValues(_.size)

    (1 to i).foldLeft(initialSequenceCounts) { (currIterMap, x) =>
      currIterMap.foldLeft(Map.empty[String, Int]) { (nextIterMap, entry) =>
        val (seq, count) = entry
        nextIterMap ++ arrowSeqMap(seq).map(s => (s, count + nextIterMap.getOrElse(s, 0))).toMap
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val codes = "029A\n980A\n179A\n456A\n379A".split("\n")
    val codes2 = "805A\n964A\n459A\n968A\n671A".split("\n")

    val finalOutput = codes.map { code =>
      println(code)
      val p1 = keypadPresses(digits, code)
      println(p1)

      val filtered = p1.filter(s => s.sliding(2).forall(arrowMap.contains))
      println(filtered)
      filtered.map(p => expand("A" + p, 3)).map(m => m.length).sum
    }
    val result = codes.zip(finalOutput).map { case (code, output) =>
      println(code, output)
      output * code.substring(0, 3).toInt
    }.sum
    println(result)
  }
}
