import scala.annotation.tailrec

object p24 {

  val sampleInput = """x00: 1
                      |x01: 0
                      |x02: 1
                      |x03: 1
                      |x04: 0
                      |y00: 1
                      |y01: 1
                      |y02: 1
                      |y03: 1
                      |y04: 1
                      |
                      |ntg XOR fgs -> mjb
                      |y02 OR x01 -> tnw
                      |kwq OR kpj -> z05
                      |x00 OR x03 -> fst
                      |tgd XOR rvg -> z01
                      |vdt OR tnw -> bfw
                      |bfw AND frj -> z10
                      |ffh OR nrd -> bqk
                      |y00 AND y03 -> djm
                      |y03 OR y00 -> psh
                      |bqk OR frj -> z08
                      |tnw OR fst -> frj
                      |gnj AND tgd -> z11
                      |bfw XOR mjb -> z00
                      |x03 OR x00 -> vdt
                      |gnj AND wpb -> z02
                      |x04 AND y00 -> kjc
                      |djm OR pbm -> qhw
                      |nrd AND vdt -> hwm
                      |kjc AND fst -> rvg
                      |y04 OR y02 -> fgs
                      |y01 AND x02 -> pbm
                      |ntg OR kjc -> kwq
                      |psh XOR fgs -> tgd
                      |qhw XOR tgd -> z09
                      |pbm OR djm -> kpj
                      |x03 XOR y03 -> ffh
                      |x00 XOR y04 -> ntg
                      |bfw OR bqk -> z06
                      |nrd XOR fgs -> wpb
                      |frj XOR qhw -> z04
                      |bqk OR frj -> z07
                      |y03 OR x01 -> nrd
                      |hwm AND bqk -> z03
                      |tgd XOR rvg -> z12
                      |tnw OR pbm -> gnj
                      |""".stripMargin

  @tailrec
  def evaluate(instructions: Map[String, String], state: Map[String, Int]): Map[String, Int] = {
    // evaluate all possible instructions
    val nextState = instructions.foldLeft(state) { (map, kv) =>
      val target = kv._1
      val instruction = kv._2
      val tokens = instruction.split(" ")
      val (arg1, arg2) = (tokens(0), tokens(2))
      val op = tokens(1)
      if (map.contains(arg1) && map.contains(arg2)) {
        val value = op match {
          case "AND" => map(arg1) & map(arg2)
          case "OR" => map(arg1) | map(arg2)
          case "XOR" => map(arg1) ^ map(arg2)
        }
        map + (target -> value)
      } else map
    }

    if (nextState.size > state.size) {
      // run another iteration
      evaluate(instructions, nextState)
    } else {
      nextState
    }
  }

  def printInstruction(instructions: Map[String, String], key: String, maxLevels: Int = 1): Unit = {
    val tokens = instructions(key).split(" ")
    val (arg1, arg2) = (tokens(0), tokens(2))
    val op = tokens(1)
    println(s"$key -> ${instructions(key)}")
    if (maxLevels > 0) {
      if (instructions.contains(arg1)) printInstruction(instructions, arg1, maxLevels - 1)
      if (instructions.contains(arg2)) printInstruction(instructions, arg2, maxLevels - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    //val input = sampleInput
    val input = scala.io.Source.fromFile("src/resources/p24.txt").mkString
    val split = input.split("\n\n")
    val initialState = split(0).split("\n").map { line =>
      val Array(k, v) = line.split(": ")
      k -> v.toInt
    }.toMap
    val instructions = split(1).split("\n").map { line =>
      val Array(left, right) = line.split(" -> ")
      right -> left
    }.toMap

    val zKeys = instructions.keys.filter(_.startsWith("z")).toSeq.sorted

    // part 1
    val result = evaluate(instructions, initialState)
    val sum: Long = result.filter(_._1.startsWith("z")).foldLeft(0L) { (sum: Long, kv) =>
      val bitPos = kv._1.drop(1).toInt
      sum + (kv._2.toLong << bitPos)
    }
    println(sum)


    // part 2 - organize rules and then eyeball finding the mixed up ones
    // example of pattern
    // fkm XOR hvb -> z02 (output bit)
    // y02 XOR x02 -> hvb (XOR inputs)
    // hqg OR cff -> fkm
    // where cff is AND of two things involved in last bit and hqg is AND of last two input bits

    zKeys.foreach { z =>
      printInstruction(instructions, z, 2)
      println("")
    }
  }
}
