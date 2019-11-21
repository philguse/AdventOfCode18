package Day02

import scala.annotation.tailrec
import utils.Utils.readFile

object InventoryManagement extends App {
  val ids = readFile("Day02/input.txt")
  val charCounts = getCharCounts(ids)
  val checksum = calculateChecksum(charCounts)
  val commonLetters = findCommonLetters(ids)

  // Results
  println(s"Part 01: $checksum")
  println(s"Part 02: $commonLetters")

  def getCharCounts(ids: List[String]): List[Set[Int]] = {
    ids.map(id =>
      id.groupBy(char => char)
        .map(_._2.length)
        .toSet
    )
  }

  def calculateChecksum(charCounts: List[Set[Int]]): Int = {
    @tailrec
    def loop(a: Int, b: Int, rest: List[Set[Int]]): Int = rest match {
      case Nil => a * b
      case x :: xs =>
        if (x.contains(2))
          if (x.contains(3)) loop(a + 1, b + 1, xs)
          else loop(a + 1, b, xs)
        else if (x.contains(3)) loop(a, b + 1, xs)
        else loop(a, b, xs)
    }

    loop(0, 0, charCounts)
  }

  def findCommonLetters(strings: List[String]): List[String] = {
    (for {
      a <- strings.indices
      b <- a + 1 until strings.length
      if getDifferenceCount(strings(a), strings(b)) == 1
    } yield getCommonLetters(strings(a), strings(b)))
      .toList
  }

  def getDifferenceCount(a: String, b: String): Int = {
    @tailrec
    def loop(count: Int, restA: String, restB: String): Int = {
      if (restA.isEmpty || restB.isEmpty) count
      else if (restA.head == restB.head) loop(count, restA.tail, restB.tail)
      else loop(count + 1, restA.tail, restB.tail)
    }

    loop(0, a, b)
  }

  def getCommonLetters(a: String, b:String): String = {
    @tailrec
    def findIndex(index: Int, restA: String, restB: String): Int = {
      if (restA.head != restB.head) index
      else findIndex(index + 1, restA.tail, restB.tail)
    }

    val n = findIndex(0, a, b)
    a.take(n) + a.drop(n + 1)
  }
}
