import scala.annotation.tailrec
import scala.io.Source

object p23 {
  val sampleInput =
    """kh-tc
      |qp-kh
      |de-cg
      |ka-co
      |yn-aq
      |qp-ub
      |cg-tb
      |vc-aq
      |tb-ka
      |wh-tc
      |yn-cg
      |kh-ub
      |ta-co
      |de-co
      |tc-td
      |tb-wq
      |wh-td
      |ta-ka
      |td-qp
      |aq-cg
      |wq-ub
      |ub-vc
      |de-ta
      |wq-aq
      |wq-vc
      |wh-yn
      |ka-de
      |kh-ta
      |co-tc
      |wh-qp
      |tb-vc
      |td-yn""".stripMargin

  @tailrec
  def findLargestClique(graph: Map[String, Array[String]], cliques: Seq[List[String]], finalClique: List[String]): List[String] = {
    if (cliques.isEmpty) finalClique
    else {
      val currClique = cliques.head
      // see if you can extend it
      val nextNodes = for {
        node <- graph.keySet if node > currClique.head
        if currClique.forall(graph(node).contains) // every node in current clique contains next node
      } yield node
      if (nextNodes.isEmpty) {
        // can't extend it, try next clique
        val nextFinal = if (currClique.size > finalClique.size) currClique else finalClique
        println("trying to extend", currClique)
        findLargestClique(graph, cliques.tail, nextFinal)
      } else findLargestClique(graph, nextNodes.toList.map(_ :: currClique) ++ cliques.tail, finalClique)
    }
  }

  def main(args: Array[String]): Unit = {
    //val input = sampleInput.split("\n").map(_.split("-"))
    val input = Source.fromFile("src/resources/p23.txt").getLines().map(_.split("-")).toArray
    val graph = input.flatMap(arr => Seq(arr(0) -> arr(1), arr(1) -> arr(0))).groupBy(_._1).mapValues(_.map(_._2))
    val nodes = graph.keySet

    val triples = for {
      n1 <- nodes
      n2 <- graph(n1) if n2 > n1
      n3 <- graph(n2) if n3 > n2 && graph(n1).contains(n3)
      if n1.startsWith("t") || n2.startsWith("t") || n3.startsWith("t")
    } yield (Seq(n1, n2, n3))
    triples.foreach(s => println(s.mkString(", ")))
    println(triples.size)

    println(findLargestClique(graph, nodes.toSeq.map(s => List(s)), Nil))
  }
}

// list = zu, zg, yx, us, tv, qy, pk, ow, gu, dt, ch, bn, aq
// reverse above ^
// reversed = aq,bn,ch,dt,gu,ow,pk,qy,tv,us,yx,zg,zu
