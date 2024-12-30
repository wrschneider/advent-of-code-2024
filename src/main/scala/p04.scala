object p04 {
  def findXmas(input: Seq[String]): Seq[(Int, Int, (Int, Int))] = {

    val allDirections = Seq((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (-1, 1), (1, -1), (-1, -1))

    // find all possible starting positions with letter X
    val startingPositions = input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.collect { case ('X', x) => (x, y) }
    }

    val list = for {
      (x, y) <- startingPositions
      direction <- allDirections if x + 3 * direction._1 >= 0 && x + 3 * direction._1 < input.head.length && y + 3 * direction._2 >= 0 && y + 3 * direction._2 < input.length
    } yield (x, y, direction)

    list.filter { case (x, y, direction) =>
      val str = (0 to 3).map { i =>
        val (dx, dy) = direction
        input(y + i * dy)(x + i * dx)
      }.mkString
      str == "XMAS"
    }
  }

  def findXShapedMas(input: Seq[String]): Seq[_] = {
    // find all possible starting positions with letter A, which would be in the center
    // of X shape
    // and 'A' must not be on edge
    val startingPositions = input.zipWithIndex.filter {
      case (_, y) => y > 0 && y < input.length - 1
    } flatMap {
      case (line, y) => line.zipWithIndex.collect {
        case ('A', x) if x > 0 && x < line.length - 1 => (x, y)
      }
    }
    startingPositions.filter { case (x, y) =>
      // "A" is center, M and S must be on other sides diagonally
      val diag1 = (input(y - 1)(x - 1), input(y + 1)(x + 1))
      val diag2 = (input(y - 1)(x + 1), input(y + 1)(x - 1))
      (diag1 == ('M', 'S') || diag1 == ('S', 'M')) && (diag2 == ('M', 'S') || diag2 == ('S', 'M'))
    }
  }

  def main(args: Array[String]): Unit = {
    val sampleInput = """MMMSXXMASM
                        |MSAMXMSMSA
                        |AMXSXMAAMM
                        |MSAMASMSMX
                        |XMASAMXAMM
                        |XXAMMXXAMA
                        |SMSMSASXSS
                        |SAXAMASAAA
                        |MAMMMXMMMM
                        |MXMXAXMASX""".stripMargin

    val input: Seq[String] = if (args.isEmpty) sampleInput.split("\n") else scala.io.Source.fromFile(args(0)).getLines().toSeq

    val strs = findXmas(input)
    println(strs.length)

    val strs2 = findXShapedMas(input)
    println(strs2.length)
  }
}
