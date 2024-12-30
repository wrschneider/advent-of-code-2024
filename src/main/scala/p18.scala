import scala.annotation.tailrec
import scala.io.Source

object p18 {
  val sampleInput = """5,4
                      |4,2
                      |4,5
                      |3,0
                      |2,1
                      |6,3
                      |2,4
                      |1,5
                      |0,6
                      |3,3
                      |2,6
                      |5,1
                      |1,2
                      |5,5
                      |2,5
                      |6,5
                      |1,4
                      |0,4
                      |6,4
                      |1,1
                      |6,1
                      |1,0
                      |0,5
                      |1,6
                      |2,0""".stripMargin.split("\n")

  type Point = (Int, Int)
  type Path = (List[Point], Int)
  val directions = Array((0, 1), (1, 0), (0, -1), (-1, 0)) // E-S-W-N, clockwise

  @tailrec
  def shortestPath(size: Int, blockedPoints: Set[Point], paths: List[Path] = List.empty,
                   history: Map[Point, Int] = Map.empty): Option[Int] = {
    if (paths.isEmpty) {
      history.get((size, size))
    } else {
      val (path, score) = paths.head
      val (r, c) = path.head
      if (r == size && c == size) {
        // made it to the end?
        //println(s"found path: $score")
        shortestPath(size, blockedPoints, paths.tail, history)
      } else {
        val nextPaths = directions.flatMap { case (dr, dc) =>
          val nextPoint = (r + dr, c + dc)
          if (nextPoint._1 >= 0 && nextPoint._1 <= size && nextPoint._2 >= 0 && nextPoint._2 <= size &&
            !blockedPoints.contains(nextPoint) &&
            (!history.contains(nextPoint) || history(nextPoint) > score + 1)) {
            Some(nextPoint :: path, score + 1)
          } else None
        }.toList
        val newHistory = history ++ nextPaths.map(p => p._1.head -> p._2)
        shortestPath(size, blockedPoints, nextPaths ++ paths.tail, newHistory)
      }
    }
  }

  @tailrec
  def firstPath(size: Int, blockedPoints: Set[Point], paths: List[Path] = List.empty,
                   history: Map[Point, Int] = Map.empty): Option[Int] = {
    if (paths.isEmpty) {
      history.get((size, size))
    } else {
      val (path, score) = paths.head
      val (r, c) = path.head
      if (r == size && c == size) {
        // made it to the end?
        //println(s"found path: $score")
        // stop looking
        history.get((size, size))
      } else {
        val nextPaths = directions.flatMap { case (dr, dc) =>
          val nextPoint = (r + dr, c + dc)
          if (nextPoint._1 >= 0 && nextPoint._1 <= size && nextPoint._2 >= 0 && nextPoint._2 <= size &&
            !blockedPoints.contains(nextPoint) && !history.contains(nextPoint)) {
            // don't bother to find cheapest path, only need to find one
            Some(nextPoint :: path, score + 1)
          } else None
        }.toList
        val newHistory = history ++ nextPaths.map(p => p._1.head -> p._2)
        firstPath(size, blockedPoints, nextPaths ++ paths.tail, newHistory)
      }
    }
  }

  def main(args: Array[String]) = {
    //val input = sampleInput
    val input = Source.fromFile("src/resources/p18.txt").getLines.toArray
    println(input.length)
    for (n <- 1024 until input.length) {
      val blockedPoints = input.map(_.split(",").map(_.toInt))
        .map(a => (a(0), a(1)))
        .take(n)
        .toSet
      println(n, input(n - 1),
        firstPath(70, blockedPoints, List((List((0, 0)), 0)))
      )
    }
  }
}
