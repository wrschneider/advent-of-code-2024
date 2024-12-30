import java.awt.GridLayout
import scala.io.Source

object p15 {

  val input3 = """#######
                 |#...#.#
                 |#.....#
                 |#..OO@#
                 |#..O..#
                 |#.....#
                 |#######
                 |
                 |<vv<<^^<<^^""".stripMargin

  val input1 = """########
                 |#..O.O.#
                 |##@.O..#
                 |#...O..#
                 |#.#.O..#
                 |#...O..#
                 |#......#
                 |########
                 |
                 |<^^>>>vv<v>>v<<""".stripMargin

  val input2 = """##########
                 |#..O..O.O#
                 |#......O.#
                 |#.OO..O.O#
                 |#..O@..O.#
                 |#O#..O...#
                 |#O..O..O.#
                 |#.OO.O.OO#
                 |#....O...#
                 |##########
                 |
                 |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
                 |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
                 |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
                 |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
                 |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
                 |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
                 |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
                 |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
                 |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
                 |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin

  val fullInput = Source.fromFile("src/resources/p15.txt").getLines().mkString("\n")


  val directionMap = Map(
    '^' -> (-1, 0),
    'v' -> (1, 0),
    '<' -> (0, -1),
    '>' -> (0, 1)
  )

  def score(grid: Array[String]): Int = {
    grid.zipWithIndex.flatMap { case (line, r) =>
      line.zipWithIndex.collect {
        case (ch, c) if ch == 'O' || ch == '[' => 100 * r + c
      }
    }.sum
  }

  def tryToMove(grid: Array[String], r: Int, c: Int, dr: Int, dc: Int): (Array[String], Int, Int) = {
    val (r1, c1) = (r + dr, c + dc)
    grid(r1)(c1) match {
      case '#' => (grid, r, c) // wall, don't move
      case '.' => // free space, direct move
        val ng = grid.updated(r1, grid(r1).updated(c1, grid(r)(c)))
        val newGrid = ng.updated(r, ng(r).updated(c, '.'))
        (newGrid, r1, c1)
      case 'O' => // box, try to push
        val (newGrid, _, _) = tryToMove(grid, r1, c1, dr, dc)
        if (newGrid(r1)(c1) == '.') {
          val ng = newGrid.updated(r1, newGrid(r1).updated(c1, newGrid(r)(c)))
          val newGrid2 = ng.updated(r, ng(r).updated(c, '.'))
          (newGrid2, r1, c1)
        } else (newGrid, r, c)
      case _ => (grid, r, c)
    }
  }

  def moveLeft(grid: Array[String], r: Int, c: Int): (Array[String], Int, Int) = {
    // find first free space to left
    grid(r).indices.filter(_ < c).reverse.find(grid(r)(_) == '.') match {
      case Some(i) => // see if there's any wall between i and c
        grid(r).indices.filter(j => j > i && j < c).find(grid(r)(_) == '#') match {
          case Some(_) => (grid, r, c) // don't move
          case None =>
            val newStr = grid(r).slice(0, i) + grid(r).slice(i + 1, c + 1) + "." + grid(r).slice(c + 1, grid(r).length)
            (grid.updated(r, newStr), r, c - 1)
        }
      case None => (grid, r, c)
    }
  }

  def moveRight(grid: Array[String], r: Int, c: Int): (Array[String], Int, Int) = {
    // find first free space to right
    grid(r).indices.filter(_ > c).find(grid(r)(_) == '.') match {
      case Some(i) => // see if there's any wall between c and i
        grid(r).indices.reverse.filter(j => j < i && j > c).find(grid(r)(_) == '#') match {
          case Some(_) => (grid, r, c) // don't move
          case None =>
            val newStr = grid(r).slice(0, c) + "." + grid(r).slice(c, i) + grid(r).slice(i + 1, grid(r).length)
            (grid.updated(r, newStr), r, c + 1)
        }
      case None => (grid, r, c)
    }
  }

  def moveRocksVertical(grid: Array[String], r: Int, c: Int, dir: Int): Option[Array[String]] = {
      // look above/below
    (grid(r + dir)(c), grid(r + dir)(c + 1)) match {
      case (_, '#') => None
      case ('#', _) => None

      case (ch1, ch2) => // one or two blocks to move
        val blocksToMove = Seq(
          if (ch1 == '[') Some(r + dir, c) else None,
          if (ch1 == ']') Some(r + dir, c - 1) else None,
          if (ch2 == '[') Some(r + dir, c + 1) else None
        ).flatten

        val updatedAfterAllMoves = blocksToMove.foldLeft(Option(grid)) { (acc, block) =>
          acc match {
            case None => None
            case Some(updatedGrid) => moveRocksVertical(updatedGrid, block._1, block._2, dir)
          }
        }

        // are two spaces free now?
        updatedAfterAllMoves.flatMap { newGrid =>
          (newGrid(r + dir)(c), newGrid(r + dir)(c + 1)) match {
            case ('.', '.') => // both spots free
              val newStr = newGrid(r).slice(0, c) + ".." + newGrid(r).slice(c + 2, newGrid(r).length)
              val newStrBelow = newGrid(r + dir).slice(0, c) + "[]" + newGrid(r + dir).slice(c + 2, newGrid(r + dir).length)
              Some(newGrid.updated(r + dir, newStrBelow).updated(r, newStr))
            case _ => None
          }
        }
    }
  }

  def moveUpDown(grid: Array[String], r: Int, c: Int, dir: Int): (Array[String], Int, Int) = {
    grid(r + dir)(c) match {
      case '#' => (grid, r, c)
      case '.' =>
        val newStr = grid(r).slice(0, c) + "." + grid(r).slice(c + 1, grid(r).length)
        val newStrAbove = grid(r + dir).slice(0, c) + grid(r)(c) + grid(r + dir).slice(c + 1, grid(r + dir).length)
        (grid.updated(r+dir, newStrAbove).updated(r, newStr), r+dir, c)
      case '[' => moveRocksVertical(grid, r + dir, c, dir) match {
        case Some(newGrid) =>
          val newStr = newGrid(r).slice(0, c) + "." + newGrid(r).slice(c + 1, newGrid(r).length)
          val newStrAbove = newGrid(r + dir).slice(0, c) + "@." + newGrid(r + dir).slice(c + 2, newGrid(r + dir).length)
          (newGrid.updated(r+dir, newStrAbove).updated(r, newStr), r+dir, c)
        case None => (grid, r, c)
      }
      case ']' => moveRocksVertical(grid, r + dir, c - 1, dir) match {
        case Some(newGrid) =>
          val newStr = newGrid(r).slice(0, c) + "." + newGrid(r).slice(c + 1, newGrid(r).length)
          val newStrAbove = newGrid(r + dir).slice(0, c-1) + ".@" + newGrid(r + dir).slice(c + 1, newGrid(r + dir).length)
          (newGrid.updated(r + dir, newStrAbove).updated(r, newStr), r + dir, c)
        case None => (grid, r, c)
      }
    }
  }

  def tryToMovePart2(grid: Array[String], r: Int, c: Int, dir: Char) = {
    val result = dir match {
      case '<' => moveLeft(grid, r, c)
      case '>' => moveRight(grid, r, c)
      case '^' => moveUpDown(grid, r, c, -1)
      case 'v' => moveUpDown(grid, r, c, +1)
    }
    println(result._1.mkString("\n"))
    result
  }

  def part1(args: Array[String]): Unit = {
    val input = input1

    val grid = input.split("\n\n")(0).split("\n")
    val moves = input.split("\n\n")(1).split("\n").mkString

    val initialPos = grid.zipWithIndex.flatMap { case (line, r) =>
      line.zipWithIndex.collect {
        case ('@', c)  => (r, c)
      }
    }.head

    val finalState = moves.foldLeft((grid, initialPos)) { (state, ch) =>
      val nextState = tryToMove(state._1, state._2._1, state._2._2, directionMap(ch)._1, directionMap(ch)._2)
      println(nextState._1.mkString("\n"))
      (nextState._1, (nextState._2, nextState._3))
    }

    println(score(finalState._1))
  }

  def main(args: Array[String]): Unit = {
    val input = fullInput

    val grid = input.split("\n\n")(0).split("\n").map(_.flatMap(c => if (c == 'O') "[]" else if (c == '@') "@." else Seq(c, c)))
    val moves = input.split("\n\n")(1).split("\n").mkString

    val initialPos = grid.zipWithIndex.flatMap { case (line, r) =>
      line.zipWithIndex.collect {
        case ('@', c)  => (r, c)
      }
    }.head

    val finalState = moves.foldLeft((grid, initialPos._1, initialPos._2)) { (state, ch) =>
      tryToMovePart2(state._1, state._2, state._3, ch)
    }

    println(score(finalState._1))
  }


}
