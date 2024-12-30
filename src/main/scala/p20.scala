import scala.annotation.tailrec
import scala.io.Source

object p20 {

  val sampleInput = """###############
                      |#...#...#.....#
                      |#.#.#.#.#.###.#
                      |#S#...#.#.#...#
                      |#######.#.#.###
                      |#######.#.#...#
                      |#######.#.###.#
                      |###..E#...#...#
                      |###.#######.###
                      |#...###...#...#
                      |#.#####.#.###.#
                      |#.#...#.#.#...#
                      |#.#.#.#.#.#.###
                      |#...#...#...###
                      |###############""".stripMargin.split("\n")

  @tailrec
  def findPath(grid: Seq[String], paths: List[List[(Int, Int)]]): List[(Int, Int)] = {
    val path = paths.head
    val (r, c) = path.head
    if (grid(r)(c) == 'E') {
      path
    } else {
      val nextPaths = Array((0, 1), (1, 0), (0, -1), (-1, 0)).flatMap { case (dr, dc) =>
        val nextPoint = (r + dr, c + dc)
        if (nextPoint._1 >= 0 && nextPoint._1 < grid.length && nextPoint._2 >= 0 && nextPoint._2 < grid(0).length &&
          grid(nextPoint._1)(nextPoint._2) != '#' && !path.contains(nextPoint)) {
          Some(nextPoint :: path)
        } else None
      }.toList
      findPath(grid, nextPaths ::: paths.tail)
    }
  }

  def findCheats(grid: Seq[String], path: Array[(Int, Int)]): Seq[Int] = {
    // take original path and find pairs of indexes where you can splice
    for {
      i <- path.indices
      j <- path.indices
      if j - i > 50
      cheatLen = Math.abs(path(i)._1 - path(j)._1) + Math.abs(path(i)._2 - path(j)._2)
      if cheatLen <= 20
      savings = j - i - cheatLen
      if savings >= 50
    } yield {
      println(i, j)
      j - i - cheatLen
    }
  }

  def main(args: Array[String]): Unit = {
    //val grid = sampleInput
    val grid = Source.fromFile("src/resources/p20.txt").getLines().toList
    // tedious, repetitive code like this next line, is what Copilot does really well
    val start = grid.zipWithIndex.collectFirst { case (row, r) if row.contains('S') => (r, row.indexOf('S')) }.get
    val paths = List(start :: Nil)
    val path = findPath(grid, paths).toArray // re-learned hard way that random access in a List is not constant time!
    println(path.length - 1)
    val cheats = findCheats(grid, path)
    // don't understand the off by one condition
    cheats.groupBy(identity).toList.sortBy(_._1).foreach { case (k, v) =>
      println(s"Cheats for $k steps: ${v.length}")
    }
    println(cheats.count(_ >= 100))
  }

}
