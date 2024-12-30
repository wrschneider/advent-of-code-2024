import scala.io.Source

object p02 {

  def isSafe(line: Array[Int]): Boolean = {
    // Copilot came up with `sliding` idea but behaved oddly because I didn't realize it was
    // an Iterator and would behave differently when iterated multiple times
    //   (adding println changed results)
    val differences = line.sliding(2).map { case Array(a, b) => a - b }.toList
    val isSafe = (differences.forall(_ > 0) || differences.forall(_ < 0)) &&
        differences.forall(d => Math.abs(d) <= 3)
    isSafe
  }

  def isSafeOneRemoved(line: Array[Int]): Boolean = {
    // brute force, took a few tries with Copilot to get this:
    line.indices.exists { i => isSafe(line.slice(0, i) ++ line.slice(i + 1, line.length)) }
  }

  def main(args: Array[String]): Unit = {
    val sampleInput = """7 6 4 2 1
                  |1 2 7 8 9
                  |9 7 6 2 1
                  |1 3 2 4 5
                  |8 6 4 4 1
                  |1 3 6 7 9""".stripMargin

    val input: Seq[String] = if (args.isEmpty) sampleInput.split("\n") else Source.fromFile(args(0)).getLines().toSeq

    val lines: Seq[Array[Int]] = input.map(_.split("\\s+").map(_.toInt))

    //lines.map(isSafe).foreach(println)
    println(lines.map(isSafeOneRemoved).count(_ == true))
  }

}
