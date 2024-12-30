object p07 {

  def allPossibleResults(numbers: List[Long]): List[Long] = {
    numbers match {
      case x :: y :: xs =>
        allPossibleResults(x + y :: xs) ++
          allPossibleResults(x * y :: xs) ++
          allPossibleResults((x.toString + y.toString).toLong :: xs)
      case x :: _ => List(x)
    }
  }

  def main(args: Array[String]): Unit = {
    val sampleInput = """190: 10 19
                        |3267: 81 40 27
                        |83: 17 5
                        |156: 15 6
                        |7290: 6 8 6 15
                        |161011: 16 10 13
                        |192: 17 8 14
                        |21037: 9 7 18 13
                        |292: 11 6 16 20""".stripMargin.split("\n")

    val realInput = scala.io.Source.fromFile("src/resources/p07.txt").getLines()
    val parsedInput = realInput.map(_.split(": "))
      .map(arr => (arr(0).toLong, arr(1).split(" ").map(_.toLong)))

    val matchingResults = parsedInput.filter(line =>
      allPossibleResults(line._2.toList).contains(line._1)
    )
    println(matchingResults.map(_._1).sum)
  }
}
