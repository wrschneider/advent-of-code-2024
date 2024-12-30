import scala.annotation.tailrec
import scala.io.Source

object p19 {

  val sampleInput = """r, wr, b, g, bwu, rb, gb, br
                      |
                      |brwrr
                      |bggr
                      |gbbr
                      |rrbgbr
                      |ubwu
                      |bwurrg
                      |brgr
                      |bbrgwb""".stripMargin

  @tailrec
  def isPossible(availableTowels: Array[String], goal: String, stack: List[String], prefixes: Set[String] = Set.empty): Boolean = {
    if (stack.isEmpty) false
    else {
      val current = stack.head
      if (current == goal) true
      else {
        val nextPatterns = availableTowels.flatMap { towel =>
          val nextPattern = current + towel
          if (!prefixes.contains(nextPattern) && nextPattern.length <= goal.length && goal.startsWith(nextPattern)) {
            Some(nextPattern)
          } else None
        }.toList
        isPossible(availableTowels, goal, nextPatterns ::: stack.tail, prefixes ++ nextPatterns.toSet)
      }
    }
  }

  def countRecursive(towels: Seq[String], goal: String, memo: Map[String, Long] = Map.empty): Map[String, Long] = {
    if (memo.contains(goal)) memo
    else if (goal.isEmpty) memo + (goal -> 1L)
    else {
      val next = towels.filter(goal.startsWith).foldLeft((0L, memo)) { (acc, towel) =>
        val (sum, map) = acc
        val nextGoal = goal.drop(towel.length)
        val result = countRecursive(towels, nextGoal, map)
        val nextSum = sum + result(nextGoal)
        (nextSum, result)
      }
      next._2 + (goal -> next._1)
    }
  }

  def part1(args: Array[String]): Unit = {
    val lines = sampleInput.split("\n\n")
    //val lines = Source.fromFile("src/resources/p19.txt").getLines().mkString("\n").split("\n\n")
    val availableTowels = lines(0).split(", ").map(_.trim)
    val patterns = lines(1).split("\n").map(_.trim)
    // part 1
    val result = patterns.foldLeft(0) { (sum, pattern) =>
      val result = isPossible(availableTowels, pattern, List(""))
      println(pattern, result)
      sum + (if (result) 1 else 0)
    }
    println(result)
  }

  def main(args: Array[String]): Unit = {
    //val lines = sampleInput.split("\n\n")
    val lines = Source.fromFile("src/resources/p19.txt").getLines().mkString("\n").split("\n\n")
    val availableTowels = lines(0).split(", ").map(_.trim)
    val patterns = lines(1).split("\n").map(_.trim)

    val result = patterns.foldLeft(0L) { (sum, pattern) =>
      println(pattern)
      val initialQueue = availableTowels.filter(pattern.startsWith)
      val initialMap = initialQueue.map(_ -> 1).toMap
      //val boolResult = isPossible(availableTowels, pattern, List(""))
      //val result = countOptions(availableTowels, pattern, initialQueue.toList, initialMap)
      val result = countRecursive(availableTowels, pattern)
      val intResult = result.getOrElse(pattern, 0L)
      println(pattern, intResult)
      sum + intResult
    }
    println(result)
  }

}
