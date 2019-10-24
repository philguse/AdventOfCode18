package Day04

import scala.annotation.tailrec
import java.util.Date
import java.text.SimpleDateFormat
import utils.Utils.readFile

case class Entry(date: Date, message: String)
case class Shift(guardId: String, timeAsleep: Int, minutesSlept: List[Int])

object ReposeRecord {
  def main(args: Array[String]): Unit = {
    val lines = readFile("src/main/scala/Day04/input.txt")
    val entries = convertToEntries(lines)
    val sortedEntries = sortEntries(entries)
    val shifts = convertToShifts(sortedEntries)
    val groupedShifts = groupShifts(shifts)
    val mostSleepingGuard = findMostSleepingGuard(groupedShifts)
    println("Most sleeping guard: #" + mostSleepingGuard)
    val mostSleptMinute = getMostSleptMinute(groupedShifts, mostSleepingGuard)
    val result = mostSleepingGuard.toInt * mostSleptMinute
    println("Result: " + mostSleepingGuard + " * " + mostSleptMinute + " = " + result)
  }

  private def convertToEntries(lines: List[String]): List[Entry] = {
    def convertStringToDate(s: String): Date = {
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")
      dateFormat.parse(s)
    }

    lines.map(line => {
      val dateAndMessage = line.split("]")
      val date = dateAndMessage(0).substring(1)
      val message = dateAndMessage(1).trim
      Entry(convertStringToDate(date), message)
    })
  }

  private def sortEntries(entries: List[Entry]): List[Entry] = entries.sortBy(_.date)

  private def convertToShifts(entries: List[Entry]): List[Shift] = {
    @tailrec
    def loop(remaining: List[Entry], currentGuard: String, timeAsleep: Int, minutesSlept: List[Int], result: List[Shift]): List[Shift] = remaining match {
      case Nil => result :+ Shift(currentGuard, timeAsleep, minutesSlept)
      case head :: rest if head.message.startsWith("Guard") =>
        if (currentGuard.isEmpty)
          loop(rest, getGuardId(head.message), 0, List(), result)
        else loop(rest, getGuardId(head.message), 0, List(), result :+ Shift(currentGuard, timeAsleep, minutesSlept))
      case head :: rest =>
        val fallAsleepTime = head.date
        val awakeningTime = rest.head.date
        loop(rest.tail, currentGuard, timeAsleep + getTimeAsleep(fallAsleepTime, awakeningTime), minutesSlept ++ getMinutesSlept(fallAsleepTime, awakeningTime), result)
    }

    loop(entries, "", 0, List(), List())
  }

  private def getTimeAsleep(a: Date, b: Date): Int = {
    val dateFormat = new SimpleDateFormat("mm")
    Math.abs(dateFormat.format(a).toInt - dateFormat.format(b).toInt)
  }

  private def getMinutesSlept(fallAsleepTime: Date, awakeningTime: Date): List[Int] = {
    val dateFormat = new SimpleDateFormat("mm")
    (dateFormat.format(fallAsleepTime).toInt until dateFormat.format(awakeningTime).toInt).toList
  }

  private def getGuardId(message: String): String = message.replaceAll("[^\\d]", "")

  private def groupShifts(shifts: List[Shift]): Map[String, List[Shift]] = shifts.groupBy(_.guardId)

  private def findMostSleepingGuard(shiftsPerGuard: Map[String, List[Shift]]): String = {
    val countTimeAsleep = shiftsPerGuard.map(entry => (entry._1, entry._2.foldLeft(0)(_ + _.timeAsleep)))
    countTimeAsleep.maxBy(_._2)._1
  }

  private def getMostSleptMinute(groupedShifts: Map[String, List[Shift]], guardId: String): Int = {
    val shifts = groupedShifts(guardId)
    val countMinutes = shifts.flatMap(_.minutesSlept).groupBy(minute => minute)
    countMinutes.maxBy(_._2.length)._1
  }
}
