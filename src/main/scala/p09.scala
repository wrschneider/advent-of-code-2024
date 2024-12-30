import scala.annotation.tailrec
import scala.io.Source

object p09 {

  @tailrec
  def defrag(storage: Array[Option[Int]], firstEmpty: Int, lastNonEmpty: Int): Array[Option[Int]] = {
    val updated = storage.slice(0, firstEmpty) ++ Array(storage(lastNonEmpty)) ++
      storage.slice(firstEmpty + 1, lastNonEmpty)
    val nextEmpty = updated.indexWhere(_.isEmpty, firstEmpty)
    val nextNonEmpty = updated.lastIndexWhere(_.nonEmpty)
    println(nextEmpty, nextNonEmpty)

    if (nextEmpty == -1 || nextNonEmpty < nextEmpty)
      storage
    else
      defrag(updated, nextEmpty, nextNonEmpty)
  }

  def part1(args: Array[String]): Unit = {
    //val input = "2333133121414131402"
    val input = Source.fromFile("src/resources/p09.txt").getLines().mkString
    val storage = input.indices.filter(_ % 2 == 0).foldLeft(Array.empty[Option[Int]]) { (acc, x) =>
      val fileId = x / 2
      val emptyBlocks = if (x == input.length - 1) Array.empty[Option[Int]] else Array.fill(input(x + 1).asDigit)(None)
      acc ++ Array.fill(input(x).asDigit)(Some(fileId)) ++ emptyBlocks
    }
    val firstEmpty = storage.indexWhere(_.isEmpty)
    val lastNonEmpty = storage.length - 1

    println(storage.length, firstEmpty, lastNonEmpty)
    val result = defrag(storage, firstEmpty, lastNonEmpty)

    val checksum = result.flatten.zipWithIndex.map { case (x, i) => i * x.toLong }.sum
    println(checksum)
  }

  @tailrec
  def defragWholeFiles(storage: IndexedSeq[(Int, Int, Int)], fileId: Int): IndexedSeq[(Int, Int, Int)] = {
    fileId match {
      case 0 => storage
      case fileId: Int =>
        val i = storage.indexWhere(_._1 == fileId)
        val (movingId, movingLen, movingFree) = storage(i)
        val firstFree = storage.indexWhere(_._3 >= movingLen)
        if (firstFree == -1 || firstFree >= i) defragWholeFiles(storage, fileId - 1)
        else {
          val (beforeId, beforeLen, beforeFree) = storage(firstFree)
          val updateMiddle = if (firstFree != i - 1) {
            // 3 total blocks impacted: the one we're moving, the one that WAS before it, and the
            // one that WILL BE before it after the move
            val (precedingId, precedingLen, precedingFree) = storage(i - 1)

            Array((beforeId, beforeLen, 0), (movingId, movingLen, beforeFree - movingLen)) ++
            storage.slice(firstFree + 1, i - 1) ++
            Array((precedingId, precedingLen, precedingFree + movingLen + movingFree))
          } else {
            // only two blocks impacted: special case condensing free space between two blocks
            // NOT covered by sample test case
            Array((beforeId, beforeLen, 0), (movingId, movingLen, beforeFree + movingFree))
          }
          val updated = storage.slice(0, firstFree) ++ updateMiddle ++ storage.slice(i + 1, storage.length)


          defragWholeFiles(updated, fileId - 1)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    //val input = "23331331214141314020"
    //val input = "232350"
    val input = Source.fromFile("src/resources/p09.txt").getLines().mkString + "0"
    val storage = input.indices.filter(_ % 2 == 0).map { x => (x / 2, input(x).asDigit, input(x+1).asDigit) }
    println(storage)
    println("Length: " + storage.length)
    println("Min / Max ID: " + storage.map(_._1).min + " " + storage.map(_._1).max)
    println("Total blocks occupied:" + storage.map(_._2).sum)
    println("Total free blocks:" + storage.map(_._3).sum)

    val result = defragWholeFiles(storage, storage.length - 1)
    val score = result.foldLeft((0, 0L)) { case ((start: Int, sum: Long), (id, len, empty)) =>
      val currSum: Long = (start until start + len).map(_.toLong * id.toLong).sum
      (start + len + empty, sum + currSum)
    }

    println(result)
    println(score)
  }
}
