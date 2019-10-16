package Day01

import scala.annotation.tailrec
import scala.io.Source

object ChronalCalibration {
  def main(args: Array[String]): Unit = {
    val frequencies = readFile("src/main/scala/Day01/input.txt")
    val frequency = calculateFrequency(frequencies)
    val frequencyTwice = calculateFrequencyTwice(frequencies)
    println("Part one: " + frequency)
    println("Part two: " + frequencyTwice)
  }

  def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    lines
  }

  def calculateFrequency(list: List[String]): Int =
    list.foldLeft(0){_ + _.toInt}

  def calculateFrequencyTwice(list: List[String]): Int = {
    @tailrec
    def loop(reachedFrequencies: Set[Int], acc: Int, index: Int): Int = {
      if (reachedFrequencies.contains(acc)) acc
      else if (index + 1 < list.length) loop(reachedFrequencies + acc, acc + list(index).toInt, index + 1)
      else loop(reachedFrequencies + acc, acc + list(index).toInt, 0)
    }

    loop(Set(), 0, 0)
  }
}
