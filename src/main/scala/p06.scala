import scala.annotation.tailrec

case class GuardState(r: Int, c: Int, dr: Int, dc: Int)

// turning
object p06 {

  @tailrec
  def countSteps(gs: GuardState, grid: Seq[String], squaresVisited: Set[(Int,Int)]): Set[(Int,Int)] = {
    (gs.r + gs.dr, gs.c + gs.dc) match {
      // stop if we leave the grid
      case (r, c) if r < 0 || r >= grid.length || c < 0 || c >= grid.head.length => squaresVisited
      case (r, c) if grid(r)(c) == '#' => // don't move, turn 90 degrees right
        countSteps(gs.copy(dr = gs.dc, dc = -gs.dr), grid, squaresVisited)
      case (r, c) => // move forward
        countSteps(gs.copy(r = r, c = c), grid, squaresVisited + ((r, c)))
    }
  }

  @tailrec
  def testLoop(gs: GuardState, grid: Seq[String], previousStates: Set[GuardState]): Boolean = {
    val forwardState = gs.copy(r = gs.r + gs.dr, c = gs.c + gs.dc)

    forwardState match {
      // stop if we leave the grid
      case GuardState(r, c, _, _) if r < 0 || r >= grid.length || c < 0 || c >= grid.head.length => false
      case _ if previousStates.contains(forwardState) => true
      case GuardState(r, c, _, _) if grid(r)(c) == '#' => // don't move, turn 90 degrees right
        testLoop(gs.copy(dr = gs.dc, dc = -gs.dr), grid, previousStates)
      case _ => testLoop(forwardState, grid, previousStates + forwardState)
    }
  }


  def main(args: Array[String]): Unit = {
    val sampleInput = """....#.....
                        |.........#
                        |..........
                        |..#.......
                        |.......#..
                        |..........
                        |.#..^.....
                        |........#.
                        |#.........
                        |......#...""".stripMargin

    val input: Seq[String] = if (args.isEmpty) sampleInput.split("\n") else scala.io.Source.fromFile(args(0)).getLines().toSeq
    //val input: Seq[String] = sampleInput.split("\n")
    val guardState = input.zipWithIndex.flatMap { case (line, r) =>
      line.zipWithIndex.collect {
        case ('^', c) => GuardState(r, c, -1, 0)
        case ('v', c) => GuardState(r, c, 1, 0)
        case ('<', c) => GuardState(r, c, 0, -1)
        case ('>', c) => GuardState(r, c, 0, 1)
      }
    }.head

    println(guardState)
    println(countSteps(guardState, input, Set((guardState.r, guardState.c))).size)

    // TODO probably a better way to do this, slow brute force but it worked
    val foo = for {
      r <- input.indices
      c <- input.head.indices if input(r)(c) == '.'
    } yield {
      val updatedMap: Seq[String] = input.updated(r, input(r).updated(c, '#'))
      testLoop(guardState, updatedMap, Set(guardState))
    }
    println(foo.count(_ == true))
  }
}
