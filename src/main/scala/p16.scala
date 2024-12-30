import scala.annotation.tailrec
import scala.io.Source

object p16 {

  val sampleInput1 =
    """###############
      |#.......#....E#
      |#.#.###.#.###.#
      |#.....#.#...#.#
      |#.###.#####.#.#
      |#.#.#.......#.#
      |#.#.#####.###.#
      |#...........#.#
      |###.#.#####.#.#
      |#...#.....#.#.#
      |#.#.#.###.#.#.#
      |#.....#...#.#.#
      |#.###.#.#.#.#.#
      |#S..#.....#...#
      |###############
      |""".stripMargin.split("\n")

  val sampleInput2 =
    """#################
      |#...#...#...#..E#
      |#.#.#.#.#.#.#.#.#
      |#.#.#.#...#...#.#
      |#.#.#.#.###.#.#.#
      |#...#.#.#.....#.#
      |#.#.#.#.#.#####.#
      |#.#...#.#.#.....#
      |#.#.#####.#.###.#
      |#.#.#.......#...#
      |#.#.###.#####.###
      |#.#.#...#.....#.#
      |#.#.#.#####.###.#
      |#.#.#.........#.#
      |#.#.#.#########.#
      |#S#.............#
      |#################""".stripMargin.split("\n")

  type Point = (Int, Int)
  type Path = (List[Point], Point, Int)
  val directions = Array((0, 1), (1, 0), (0, -1), (-1, 0)) // E-S-W-N, clockwise

  @tailrec
  def findBestPath(grid: Seq[String], paths: List[Path], history: Map[(Point, Point), Int] = Map.empty,
                   bestScore: Option[Int] = None, bestMap: Map[Int, Set[Point]]=Map.empty): Option[Int] = {
    if (paths.isEmpty) bestScore
    else {
      //println(paths.length)
      val (path, facing, score) = paths.head

      val (r, c) = path.head
      if (grid(r)(c) == 'E') {
        if (bestScore.forall(score <= _)) {
          println(s"found path: $score")
          val nextSet = bestMap.getOrElse(score, Set.empty) ++ path.toSet
          println(nextSet.size)

          grid.zipWithIndex.foreach {
            case (line, r) =>
              println(line.zipWithIndex.map {
                case (ch, c) =>
                  if (nextSet.contains((r, c))) 'O'
                  else ch
              }.mkString)
          }

          findBestPath(grid, paths.tail, history, Some(score), bestMap + (score -> nextSet))
        } else {
          // this branch never seems to be taken anymore
          println(s"found path: $score > $bestScore")
          findBestPath(grid, paths.tail, history, bestScore, bestMap)
        }
      } else if (bestScore.exists(score > _)) {
        //println(s"score for path already > $bestScore, pruning")
        findBestPath(grid, paths.tail, history, bestScore, bestMap)
      } else {
        val right = directions((directions.indexOf(facing) + 1) % 4)
        val left = directions((directions.indexOf(facing) + 3) % 4)
        // can go forward, left or right at different cost
        // note that history of best scores to reach a given point have to take direction facing
        // into account, not just position!
        val newPaths = List((facing, score + 1), (right, score + 1001), (left, score + 1001))
          .flatMap { case (newDir, newScore) =>
            val newPos = (r + newDir._1, c + newDir._2)
            val (newR, newC) = newPos
            if (grid(newR)(newC) != '#'
              && (!history.contains((newPos, newDir)) || newScore <= history(newPos, newDir))
            ) Some((newPos :: path, newDir, newScore)) else None
          }
        val newHistory = history ++ newPaths.map(p => (p._1.head, p._2) -> p._3)
        findBestPath(grid, newPaths ++ paths.tail, newHistory, bestScore, bestMap)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //val input = sampleInput2
    val input = Source.fromFile("src/resources/p16.txt").getLines().toList
    val startingPos = input.zipWithIndex.flatMap { case (line, r) =>
      line.zipWithIndex.collect {
        case ('S', c) => (r, c)
      }
    }.head

    val t1 = System.currentTimeMillis()

    // part 2 shaves off a few seconds if pre-seed with best score from part 1
    findBestPath(input, List((List(startingPos), directions(0), 0)), bestScore = Some(135536)) match {
      case Some(score) => println(score)
      case None => println("No path found")
    }
    val t2 = System.currentTimeMillis()
    // this line courtesy of copilot
    println(s"Elapsed time: ${t2 - t1} ms")
  }
}
