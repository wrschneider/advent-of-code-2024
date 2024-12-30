object p25 {

  val sampleInput = """#####
                      |.####
                      |.####
                      |.####
                      |.#.#.
                      |.#...
                      |.....
                      |
                      |#####
                      |##.##
                      |.#.##
                      |...##
                      |...#.
                      |...#.
                      |.....
                      |
                      |.....
                      |#....
                      |#....
                      |#...#
                      |#.#.#
                      |#.###
                      |#####
                      |
                      |.....
                      |.....
                      |#.#..
                      |###..
                      |###.#
                      |###.#
                      |#####
                      |
                      |.....
                      |.....
                      |.....
                      |#....
                      |#.#..
                      |#.#.#
                      |#####""".stripMargin

  def main(args: Array[String]): Unit = {
    //val input = sampleInput
    val input = scala.io.Source.fromFile("src/resources/p25.txt").mkString
    val split = input.split("\n\n")
    val keysAndLocks = split.map(_.split("\n"))
    val locks = keysAndLocks.filter(_.head == "#####")
    val keys = keysAndLocks.filter(_.head == ".....")

    val lockHeights = locks.map { lock =>
      (0 to 4).map(i => lock.lastIndexWhere(_(i) == '#'))
    }
    val keyHeights = keys.map { key =>
      (0 to 4).map(i => 6 - key.indexWhere(_(i) == '#'))
    }

    val matchingPairs = for {
      lock <- lockHeights
      key <- keyHeights
      if (0 to 4).forall(i => lock(i) + key(i) <= 5)
    } yield (lock, key)

    println(matchingPairs.length)

  }

}
