import scala.io.Source

object p10 {

  type Path = List[(Int, Int)]

  def walkPaths(grid: Seq[Seq[Int]], paths: Seq[Path]): Seq[Path] = {
    val (completePaths, incompletePaths) = paths.partition {
      case (r,c) :: xs if grid(r)(c) == 9 => true
      case _ => false
    }

    // try to extend incomplete paths
    val newPaths = incompletePaths.flatMap { path =>
      val (r, c) = path.head
      val nextSteps = Seq((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1))
      nextSteps
        .filter { case (r2, c2) => r2 >= 0 && c2 >= 0 && r2 < grid.length && c2 < grid.head.length }
        .filter { case (r2, c2) => grid(r2)(c2) == grid(r)(c) + 1 }
        .map { rc => rc :: path }
    }

    if (newPaths.isEmpty) {
      completePaths
    } else {
      walkPaths(grid, completePaths ++ newPaths)
    }
  }
  def main(args: Array[String]): Unit = {
    val input2 = """89010123
                  |78121874
                  |87430965
                  |96549874
                  |45678903
                  |32019012
                  |01329801
                  |10456732
                  |""".stripMargin.split("\n")

    val input = Source.fromFile("src/resources/p10.txt").getLines().toSeq
    val grid = input.map(_.map(_.asDigit))

    val trailheads = grid.zipWithIndex.flatMap { case (row, r) =>
      row.zipWithIndex.collect { case (0, c) => (r, c) }
    }
    trailheads.foreach(println)

    val pathsFromTrailhead = trailheads.map(head => walkPaths(grid, Seq(List(head))))
    val distinctEndpoints = pathsFromTrailhead.map(_.map(_.head).distinct)
    println(distinctEndpoints.map(_.length).sum) // part 1
    // part 2 counts all distinct paths rather than distinct reachable endpoints
    println(pathsFromTrailhead.map(_.length).sum)
  }

}
