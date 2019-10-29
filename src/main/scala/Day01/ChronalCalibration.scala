package Day01

import scala.annotation.tailrec
import utils.Utils.readFile

object ChronalCalibration extends App {
  val frequencies = readFile("Day01/input.txt")
  val frequency = calculateFrequency(frequencies)
  val frequencyTwice = calculateFrequencyThatOccursTwice(frequencies)
  println("Part one: " + frequency)
  println("Part two: " + frequencyTwice)

  def calculateFrequency(list: List[String]): Int = list.foldLeft(0){_ + _.toInt}

  def calculateFrequencyThatOccursTwice(list: List[String]): Int = {
    @tailrec
    def loop(reachedFrequencies: Set[Int], currentFrequency: Int, index: Int): Int = {
      if (reachedFrequencies.contains(currentFrequency)) currentFrequency
      else if (index + 1 < list.length) loop(reachedFrequencies + currentFrequency, currentFrequency + list(index).toInt, index + 1)
      else loop(reachedFrequencies + currentFrequency, currentFrequency + list(index).toInt, 0)
    }

    loop(Set(), 0, 0)
  }
}
