import scala.annotation.tailrec

object p17 {

  case class State(a: Long, b: Long=0L, c: Long=0L, ip: Int=0, output: Seq[Int] = Seq.empty)

  val input1 = """Register A: 729
                |Register B: 0
                |Register C: 0
                |
                |Program: 0,1,5,4,3,0""".stripMargin

  val input = """Register A: 30118712
                |Register B: 0
                |Register C: 0
                |
                |Program: 2,4,1,3,7,5,4,2,0,3,1,5,5,5,3,0""".stripMargin

  def combo(operand: Int, state: State): Long = {
    operand match {
      case i: Int if i >= 0 && i <= 3 => i
      case 4 => state.a
      case 5 => state.b
      case 6 => state.c
      case _ => throw new IllegalArgumentException("Invalid operand")
    }
  }
  @tailrec
  def runProgram(state: State, program: Array[Int]): State = {
    if (state.ip >= program.length - 1) state
    else {
      val opcode = program(state.ip)
      val operand = program(state.ip + 1)
      val newState = opcode match {
        case 0 => state.copy(a = state.a >> combo(operand, state), ip = state.ip + 2)
        case 1 => state.copy(b = state.b ^ operand, ip = state.ip + 2)
        case 2 => state.copy(b = combo(operand, state) % 8, ip = state.ip + 2)
        case 3 => state.copy(ip = if (state.a == 0) state.ip + 2 else operand)
        case 4 => state.copy(b = state.b ^ state.c, ip = state.ip + 2)
        case 5 => state.copy(output = state.output :+ ((combo(operand, state) % 8)).toInt, ip = state.ip + 2)
        case 6 => state.copy(b = state.a >> combo(operand, state), ip = state.ip + 2)
        case 7 => state.copy(c = state.a >> combo(operand, state), ip = state.ip + 2)
      }
      runProgram(newState, program)
    }
  }
  @tailrec
  def workBackwards(program: Array[Int], digit: Int, start: Seq[Long]): Long = {
    if (digit >= program.length) start.min else {
      println(digit, start.length)
      val next = (0 to 7).flatMap { i =>
        val out = start.flatMap { j =>
          val shl = 10 + 3 * (digit - 1)
          val a = (i.toLong << shl) | j
          val state = runProgram(State(a = a), program)
          println(a, state.output.mkString(", "))
          if (state.output.length >= digit && state.output.startsWith(program.slice(0, digit + 1)))
            Some(a)
          else None
        }
        println(out.length)
        out
      }
      workBackwards(program, digit + 1, next)
      }
  }

  def main(args: Array[String]): Unit = {
    val lines = input.split("\n")
    val registers = lines.take(3).map(_.split(": ")(1).toInt)
    val program = lines.last.replace("Program: ", "").split(",").map(_.toInt)
    val initialState = State(registers(0), registers(1), registers(2), 0)
    val finalState = runProgram(initialState, program)
    println(finalState)
    println(finalState.output.mkString(","))

    // part 2 brute force to see a pattern
    val last10 = (0L until 2048L).filter { i =>
      // last 10 bits
      val finalState = runProgram(initialState.copy(a = i), program)
      finalState.output(0) == 2
    }

    val result = workBackwards(program, 1, last10)
    println(result)
  }

}
