import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object p13 {

  type Pair = (Long, Long)

  @tailrec
  def gcd(x: Long, y: Long): Long = {
    // greatest common divisor of x and y
    if (y == 0) x else gcd(y, x % y)
  }

  def minTokensToReach(a: Pair, b: Pair, target: Pair): Option[Long] = {
    // solve for A,B - 2 equations, two unknowns, may be zero, one or more solutions
    // ax*A + bx*B = targetx
    // ay*A + by*B = targety

    val (ax, ay) = a
    val (bx, by) = b
    val (targetx, targety) = target

    // find one solution for first, could be multiple solutions though:
    // diophantine https://en.wikipedia.org/wiki/Diophantine_equation
    // turns out that the first solution IS the solution, there is no minimization
    //  to be done (b/c constraint all positive numbers?)

    val bNum = targetx*ay - targety*ax
    val bDenom = (bx*ay - by*ax)

    val oneSolution = if (bNum % bDenom != 0) {
      println(s"fail 1 no solution for $a, $b, $target")
      None
    } else {
      val bSol = bNum / bDenom
      if (bSol > 0 && (targetx - bSol*bx) % ax == 0) {
        val aSol = (targetx - bSol * bx) / ax
        println(s"solution: $aSol, $bSol")
        Some(aSol, bSol)
      } else {
        // this case was used precisely ONCE in the full input
        println(s"fail 2 no solution for $a, $b, $target")
        None
      }
    }
    oneSolution.map(s => s._1 * 3 + s._2)
  }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("src/resources/p13.txt").mkString

    val input2 = """Button A: X+94, Y+34
                  |Button B: X+22, Y+67
                  |Prize: X=8400, Y=5400
                  |
                  |Button A: X+26, Y+66
                  |Button B: X+67, Y+21
                  |Prize: X=12748, Y=12176
                  |
                  |Button A: X+17, Y+86
                  |Button B: X+84, Y+37
                  |Prize: X=7870, Y=6450
                  |
                  |Button A: X+69, Y+23
                  |Button B: X+27, Y+71
                  |Prize: X=18641, Y=10279
                  |""".stripMargin

    val pairs = input.split("\n\n").map { claw: String =>
      val clawLines = claw.split("\n")
      val r1 = "X\\+(\\d+), Y\\+(\\d+)".r
      val r2 = "X=(\\d+), Y=(\\d+)".r
      val a: Pair = r1.findFirstMatchIn(clawLines(0)) match {
        case Some(m) => (m.group(1).toLong, m.group(2).toLong)
      }
      val b: Pair = r1.findFirstMatchIn(clawLines(1)) match {
        case Some(m) => (m.group(1).toLong, m.group(2).toLong)
      }
      val plusFactor = 10000000000000L
      val target = r2.findFirstMatchIn(clawLines(2)) match {
        case Some(m) => (m.group(1).toInt + plusFactor, m.group(2).toInt + plusFactor)
      }
      (a, b, target)
    }
    val answer = pairs.map { case (a, b, target) => minTokensToReach(a, b, target) }
    answer.foreach(println)
    println(answer.flatten.sum)
  }
}

