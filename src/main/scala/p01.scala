import scala.io.Source

object p01 {
  val sampleInput = """3   4
                |4   3
                |2   5
                |1   3
                |3   9
                |3   3""".stripMargin

  def main(args: Array[String]): Unit = {
    val input = if (args.isEmpty) sampleInput.split("\n")
    else Source.fromFile(args(0)).getLines().toArray

    val lines: Array[Array[Int]] = input.map(_.split("\\s+").map(_.toInt))

    val leftColumn = lines.map(_(0)).sorted
    val rightColumn = lines.map(_(1)).sorted
    // this line came from copilot:
    // val rightFrequency = rightColumn.groupBy(identity).mapValues(_.length)

    // this is same result w/better space complexity
    val rightFrequency = rightColumn.foldLeft(Map[Int, Int]()) {
      // copilot filled this in:
      (acc, x) => acc + (x -> (acc.getOrElse(x, 0) + 1))
    }

    // copilot gave me the zip/map/case and I just filled in the Math.abs part
    val result = leftColumn.zip(rightColumn).map { case (l, r) => Math.abs(l - r) }
      .sum

    println(result)

    val part2 = leftColumn.map(x => x * rightFrequency.getOrElse(x, 0)).sum
    println(part2)
  }
}
