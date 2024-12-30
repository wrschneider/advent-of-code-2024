object p11 {

  def blink(stones: Seq[Long]): Seq[Long] = {
    stones.flatMap { stone =>
      val ss = stone.toString
      stone match {
        case s if s == 0 => Seq(1L)
        case _ if ss.length % 2 == 0 => Seq(ss.slice(0, ss.length / 2), ss.slice(ss.length / 2, ss.length)).map(_.toLong)
        case _ => Seq(stone * 2024)
      }
    }
  }

  type CachedResults = Map[(Long,Int), Long]

  def count(stone: Long, n: Int, results: CachedResults): (Long, CachedResults) = {
    if (n == 0) (1, results)
    else if (results.contains((stone, n))) (results((stone, n)), results)
    else {
      val nextStones = blink(Seq(stone)) // only 1 or 2 elements
      val (count1, results1) = count(nextStones.head, n - 1, results)
      if (nextStones.length == 2) {
        // make sure to use the results from first branch when computing second branch;
        val (count2, results2) = count(nextStones(1), n - 1, results1)
        val newResults = results2 + ((stone, n) -> (count1 + count2))
        (count1 + count2, newResults)
      } else {
        val newResults = results1 + ((stone, n) -> count1)
        (count1, newResults)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //val input = "125 17"
    val input = "4329 385 0 1444386 600463 19 1 56615"
    val stones: Seq[Long] = input.split(" ").map(_.toLong)
    val result = stones.map(count(_, 75, Map.empty)).map(_._1).sum
    println(result)
  }
}
