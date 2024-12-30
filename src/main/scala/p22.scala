import scala.io.Source
import scala.annotation.tailrec

object p22 {

  @tailrec
  def nextSecret(secret: Long, i: Int = 0, stop: Int = 2000): Long = {
    if (i >= stop) secret
    else {
      val step1 = ((secret << 6) ^ secret) & 0xFFFFFF
      val step2 = ((step1 >> 5) ^ step1) & 0xFFFFFF
      val step3 = ((step2 << 11) ^ step2) & 0xFFFFFF
      println(step3)
      nextSecret(step3, i + 1, stop)
    }
  }

  def bananasBySequence(secret: Long) = {
    val secrets = (1 to 2000).foldLeft(List(secret)) { (acc, _) =>
      val s = acc.head
      val step1 = ((s << 6) ^ s) & 0xFFFFFF
      val step2 = ((step1 >> 5) ^ step1) & 0xFFFFFF
      val step3 = ((step2 << 11) ^ step2) & 0xFFFFFF
      step3 :: acc
    }.reverse

    val prices = secrets.map(s => (s % 10).toInt)

    prices.sliding(5).foldLeft(Map.empty[(Int,Int,Int,Int), Long]) { (map, window) =>
      val tuple = (window(1)-window(0), window(2)-window(1), window(3)-window(2), window(4) - window(3))
        if (!map.contains(tuple)) map + (tuple -> (window(4)))
      else map
    }
  }

  def main(args: Array[String]): Unit = {
    val input2 =
      """1
        |2
        |3
        |2024""".stripMargin.split("\n")

    val input = Source.fromFile("src/resources/p22.txt").getLines.toArray

    //part 1
    // val result = input.map(s => nextSecret(s.toLong, 0, 2000)).sum
    val result = input.map(s => bananasBySequence(s.toLong))

    // look for all sequences that occur for at least one
    val allTuples = result.foldLeft(Set.empty[(Int ,Int, Int, Int)]) { (m, keys) =>
      m ++ keys.keySet
    }

    val mBy = allTuples.maxBy { tuple => result.map(m => m.getOrElse(tuple, 0L)).sum }
    println(mBy)
    println(result.map(m => m.getOrElse(mBy, 0L)).sum)
  }

}
