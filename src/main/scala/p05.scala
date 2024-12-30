import scala.io.Source

object p05 {

  def isValidOrder(pages: Array[Int], ruleLookup: Map[Int, Set[Int]]): Boolean = {
    pages.sliding(2).forall { case Array(a, b) =>
      ruleLookup.get(a) match {
        case Some(set) => set.contains(b)
        case None => false
      }
    }
  }

  def reorder(pages: Array[Int], ruleLookup: Map[Int, Set[Int]]): Array[Int] = {
    pages.sortWith((a, b) => {
      ruleLookup.get(a) match {
        case Some(set) => set.contains(b)
        case None => false
      }
    })
  }

  def main(args: Array[String]): Unit = {
    val sampleInput = """47|53
                        |97|13
                        |97|61
                        |97|47
                        |75|29
                        |61|13
                        |75|53
                        |29|13
                        |97|29
                        |53|29
                        |61|53
                        |97|53
                        |61|29
                        |47|13
                        |75|47
                        |97|75
                        |47|61
                        |75|61
                        |47|29
                        |75|13
                        |53|13
                        |
                        |75,47,61,53,29
                        |97,61,53,29,13
                        |75,29,13
                        |75,97,47,61,53
                        |61,13,29
                        |97,13,75,29,47""".stripMargin

    val input = Source.fromFile(args(0)).mkString

    val splitInput = input.split("\n\n")
    val ruleLookup = splitInput(0).split("\n").map(_.split("\\|").map(_.toInt))
      .groupBy(_(0))
      .mapValues(_.map(_(1)).toSet)

    val (validPrintouts, invalidPrintouts) = splitInput(1).split("\n")
      .map(_.split(",").map(_.toInt)).partition(isValidOrder(_, ruleLookup))

    val result = validPrintouts.map(arr => {
      arr(arr.length / 2)
    }).sum

    //validPrintouts.map(_.mkString(",")).foreach(println)

    println(result)

    val reordered = invalidPrintouts.map(reorder(_, ruleLookup))
    val result2 = reordered.map(arr => {
      arr(arr.length / 2)
    }).sum
    println(result2)
  }

}
