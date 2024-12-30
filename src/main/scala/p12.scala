import scala.io.Source

object p12 {

  type Point = (Int, Int)

  def contributionToPerimeter(grid: Seq[String], r: Int, c: Int): Int = {
    val height = grid.length
    val width = grid.head.length
    val dr = Seq(-1, 0, 1, 0)
    val dc = Seq(0, 1, 0, -1)
    dr.zip(dc).map { case (dr, dc) =>
      val r1 = r + dr
      val c1 = c + dc
      if (r1 < 0 || r1 >= height || c1 < 0 || c1 >= width) 1
      else if (grid(r1)(c1) != grid(r)(c)) 1
      else 0
    }.sum
  }

  def fillRegion(grid: Seq[String], p: Point, visited: Set[Point]): Set[Point] = {
    val height = grid.length
    val width = grid.head.length
    val (r, c) = p
    if (r < 0 || r >= height || c < 0 || c >= width) visited
    else if (visited.contains(p)) visited
    else {
      val newVisited = visited + p
      val dr = Seq(1, 0, -1, 0)
      val dc = Seq(0, 1, 0, -1)
      dr.zip(dc).foldLeft(newVisited) { case (visited2, (dr, dc)) =>
        if (r + dr < 0 || c + dc < 0 || r + dr >= height || c + dc >= width) visited2
        else if (visited2.contains((r + dr, c + dc))) visited2
        else if (grid(r)(c) == grid(r + dr)(c + dc)) fillRegion(grid, (r + dr, c + dc), visited2)
        else visited2
      }
    }
  }

  def countSidesInRegion(grid: Seq[String], region: Set[Point]): Int = {
    val segments = region.foldLeft((Set.empty[Point], Set.empty[Point], Set.empty[Point], Set.empty[Point])) {
      (acc, x) =>
      val (top: Set[Point], bottom: Set[Point], left: Set[Point], right: Set[Point]) = acc
      val (r, c) = x

      val newTop = if (r == 0 || !region.contains((r - 1, c))) top + ((r, c)) else top
      val newBottom = if (r == grid.length - 1 || !region.contains((r + 1, c))) bottom + ((r + 1, c)) else bottom
      val newLeft = if (c == 0 || !region.contains((r, c - 1))) left + ((r, c)) else left
      val newRight = if (c == grid.head.length - 1 || !region.contains((r, c + 1))) right + ((r, c + 1)) else right
      (newTop, newBottom, newLeft, newRight)
    }

    val top = segments._1.toSeq.groupBy { case (r, _) => r }.mapValues(_.map { case (_, c) => c }.sorted)
    val bottom = segments._2.toSeq.groupBy { case (r, _) => r }.mapValues(_.map { case (_, c) => c }.sorted)
    val left = segments._3.toSeq.groupBy { case (_, c) => c }.mapValues(_.map { case (r, _) => r }.sorted)
    val right  = segments._4.toSeq.groupBy { case (_, c) => c }.mapValues(_.map { case (r, _) => r }.sorted)

    println(top, bottom, left, right)

    Seq(top, bottom, left, right).map { edges =>
      edges.values.map { points: Seq[Int] =>
        (0 until points.length - 1).count(i => points(i + 1) - points(i) > 1) + 1
      }.sum
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val input =
      """EEEEE
        |EXXXX
        |EEEEE
        |EXXXX
        |EEEEE
        |""".stripMargin.split("\n")
    val input2 =
      """RRRRIICCFF
        |RRRRIICCCF
        |VVRRRCCFFF
        |VVRCCCJFFF
        |VVVVCJJCFE
        |VVIVCCJJEE
        |VVIIICJJEE
        |MIIIIIJJEE
        |MIIISIJEEE
        |MMMISSJEEE
        |""".stripMargin.split("\n")

    //val grid = input2
    val grid = Source.fromFile("src/resources/p12.txt").getLines().toList

    val points = for {
      r <- grid.indices
      c <- grid.head.indices
    } yield (r, c)

    val regions = points.foldLeft(Seq.empty[Set[Point]]) { (regionList, p) =>
      if (regionList.exists { region => region.contains(p) }) regionList
      else {
        val region = fillRegion(grid, p, Set.empty)
        regionList :+ region
      }
    }

    val output = regions.map {
      region =>
        //val perimeters = region.toSeq.map { case (r, c) => contributionToPerimeter(grid, r, c) }
        //val perimeter = perimeters.sum

        val edges = countSidesInRegion(grid, region)
        val area = region.size
        println(grid(region.head._1)(region.head._2), area, edges)
        area.toLong * edges.toLong
    }.sum

    println(output)
    //806222 too low
    //809992 still too low
  }
}
