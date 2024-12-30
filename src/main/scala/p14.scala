import java.io.{FileOutputStream, PrintStream}
import scala.annotation.tailrec

object p14 {

  type Pair = (Int, Int)
  case class Robot(p: Pair, v: Pair)

  val (height, width) = (103, 101)
  //val (height, width) = (7, 11)

  def correct(pos: Int, dim: Int): Int = {
    if (pos < 0) pos + dim
    else pos
  }

  def finalPosition(robot: Robot, t: Int): Pair = {
    val nextPos = (
      correct((robot.p._1 + robot.v._1 * t) % width, width),
      correct((robot.p._2 + robot.v._2 * t) % height, height)
    )
    nextPos
  }

  def quadrant(p: Pair): Int = {
    val (x, y) = p
    if (x < width / 2 && y < height / 2) 1
    else if (x > width / 2 && y < height / 2) 2
    else if (x < width / 2 && y > height / 2) 3
    else if (x > width / 2 && y > height / 2) 4
    else 0
  }

  def part1(robots: List[Robot]): Unit = {
    val finalPos = robots.map(r => finalPosition(r, 100)).sorted
    finalPos.foreach(println)
    val byQuadrant = finalPos.groupBy(quadrant).filter(_._1 > 0)
    byQuadrant.foreach(t => println(t._1 + ", " + t._2.mkString(",")))
    println(byQuadrant.map(_._2.size).product) //233895816 too high
  }

  def visualize(i: Int, robots: List[Pair]): Unit = {
    // first mutable state for AoC right here, contained within this one method
    val grid = Array.fill(height, width)(0)
    robots.foreach { robot =>
      val (x, y) = robot
      val (c, r) = (x, y) // for clarity
      grid(r)(c) = grid(r)(c) + 1
    }
    println(i)
    grid.foreach(row => println(row.mkString.map(c => if (c == '0') ' ' else c)))
  }

  def main(args: Array[String]): Unit = {
    val input =
      """p=0,4 v=3,-3
        |p=6,3 v=-1,-3
        |p=10,3 v=-1,2
        |p=2,0 v=2,-1
        |p=0,0 v=1,3
        |p=3,0 v=-2,-2
        |p=7,6 v=-1,-3
        |p=3,0 v=-1,-2
        |p=9,3 v=2,3
        |p=7,3 v=-1,2
        |p=2,4 v=2,-3
        |p=9,5 v=-3,-3""".stripMargin.split("\n")

    val input2 = scala.io.Source.fromFile("src/resources/p14.txt").getLines().toList

    val robots = input2.map { line =>
      val p = line.split(" ")(0).split("=")(1).split(",").map(_.toInt)
      val v = line.split(" ")(1).split("=")(1).split(",").map(_.toInt)
      Robot((p(0), p(1)), (v(0), v(1)))
    }

    System.setOut(new PrintStream(new FileOutputStream("file.out")))
    //part1(robots)
    for (i <- 1000 to 10000) {
      visualize(i, robots.map(finalPosition(_, i)))
    }
  }
}
