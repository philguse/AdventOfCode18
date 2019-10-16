package Day01

import scala.io.Source

object ChronalCalibration {
  def main(args: Array[String]): Unit = {
    val frequencies = readFile("src/main/scala/Day01/input.txt")
    val result = calculateFrequency(frequencies)
    println(result)
  }

  def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    lines
  }

  def calculateFrequency(list: List[String]): Int =
    list.foldLeft(0){_ + _.toInt}
}
