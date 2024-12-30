import scala.::
import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

object p08 {

  def part1(locations: Seq[(Int,Int)], height: Int, width: Int): Set[(Int,Int)] = {
    for (loc1 <- locations; loc2 <- locations if loc2 > loc1) yield {
      val (r1, c1) = loc1
      val (r2, c2) = loc2
      val dr = r2 - r1
      val dc = c2 - c1
      val candidate1 = (r2 + dr, c2 + dc)
      val candidate2 = (r1 - dr, c1 - dc)
      val an1 = if (candidate1._1 >= 0 && candidate1._1 < height && candidate1._2 >= 0 && candidate1._2 < width)
        Some(candidate1) else None
      val an2 = if (candidate2._1 >= 0 && candidate2._1 < height && candidate2._2 >= 0 && candidate2._2 < width)
        Some(candidate2) else None
      Set(an1, an2)
    }.flatten
  }.flatten.toSet

  @tailrec
  def findAntinodesFromPosition(loc1: (Int,Int), delta: (Int,Int), height: Int, width: Int, antinodes: Set[(Int, Int)]): Set[(Int,Int)] = {
    val (r1, c1) = loc1
    val (dr, dc) = delta
    val nextLoc = (r1 + dr, c1 + dc)
    if (nextLoc._1 < 0 || nextLoc._1 >= height || nextLoc._2 < 0 || nextLoc._2 >= width) antinodes
    else findAntinodesFromPosition(nextLoc, delta, height, width, antinodes + nextLoc)
  }

  def findAntinodesOneFrequency(locations: Seq[(Int,Int)], height: Int, width: Int): Set[(Int,Int)] = {
    for (loc1 <- locations; loc2 <- locations if loc2 > loc1) yield {
      val (r1, c1) = loc1
      val (r2, c2) = loc2
      val dr = r2 - r1
      val dc = c2 - c1
      findAntinodesFromPosition(loc1, (dr, dc), height, width, Set.empty) ++
        findAntinodesFromPosition(loc2, (-dr, -dc), height, width, Set.empty)
    }
  }.flatten.toSet

  def main(args: Array[String]): Unit = {
    val sampleInput = """............
                        |........0...
                        |.....0......
                        |.......0....
                        |....0.......
                        |......A.....
                        |............
                        |............
                        |........A...
                        |.........A..
                        |............
                        |............""".stripMargin.split("\n")

    //val input = sampleInput
    val input = scala.io.Source.fromFile("src/resources/p08.txt").getLines().toList

    val antennaLocations = input.zipWithIndex.flatMap { case (line, r) =>
      line.zipWithIndex.collect { case (ch, c) if ch.isLetterOrDigit => (ch, r, c) }
    }
    val height = input.length
    val width = input.head.length

    val antennasGrouped = antennaLocations.groupBy(_._1).mapValues(_.map(t => (t._2, t._3)))
    antennasGrouped.foreach(ch => println(ch._1 + " " + ch._2.mkString(",")))
    val allAntinodes = antennasGrouped.map(ch => findAntinodesOneFrequency(ch._2, height, width))
      .foldLeft(Set.empty[(Int,Int)])(_ ++ _)
    println(allAntinodes.size)
  }
}
