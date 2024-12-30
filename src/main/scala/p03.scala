import java.util.regex.Pattern
import scala.io.Source

object p03 {
  def handleInput(s: String): Int = {
    val regex = "(mul\\(\\d+,\\d+\\))|(do\\(\\))|(don't\\(\\))".r
    regex.findAllIn(s).foldLeft((0, true)) { (acc, x) =>
      (x, acc._2) match {
        case (s2: String, true) if s2.startsWith("mul") => {
          val Array(a, b) = s2.drop(4).dropRight(1).split(",")
          (acc._1 + a.toInt * b.toInt, acc._2)
        }
        case (s2: String, _) if s2.startsWith("don't") => (acc._1, false)
        case (s2: String, _) if s2.startsWith("do") => (acc._1, true)
        case _ => acc
      }
    }._1
  }

  def main(args: Array[String]): Unit = {
    val sampleInput = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

    val input = if (args.isEmpty) sampleInput else Source.fromFile(args(0)).getLines().mkString
    println(handleInput(input))
  }
}
