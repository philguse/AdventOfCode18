package Day04

import scala.annotation.tailrec
import java.util.Date
import java.text.SimpleDateFormat
import utils.Utils.readFile

case class Entry(date: Date, message: String)
case class Shift(guardId: String, timeAsleep: Int, minutesSlept: List[Int])
case class Guard(guardId: String, shifts: List[Shift])
case class Minute(value: Int, times: Int)

object ReposeRecord extends App {
  val lines = readFile("Day04/input.txt")
  val entries = convertToEntries(lines)
  val sortedEntries = sortEntries(entries)
  val shifts = convertToShifts(sortedEntries)
  val guards = assignShiftsToGuards(shifts)

  val guard1 = firstStrategy(guards).get
  val mostSleptMinute1 = getMostSleptMinute(guard1.shifts)
  val guard2 = secondStrategy(guards).get
  val mostSleptMinute2 = getMostSleptMinute(guard2.shifts)

  printResult(guard1, mostSleptMinute1)
  printResult(guard2, mostSleptMinute2)

  private def printResult(guard: Guard, minute: Minute): Unit = {
    val result = guard.guardId.toInt * minute.value
    println("Guard: #" + guard.guardId)
    println("Result: " + guard.guardId + " * " + minute.value + " = " + result)
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

  private def getGuardId(message: String): String = message.replaceAll("[^\\d]", "")

  private def getTimeAsleep(a: Date, b: Date): Int = {
    val dateFormat = new SimpleDateFormat("mm")
    Math.abs(dateFormat.format(a).toInt - dateFormat.format(b).toInt)
  }

  private def getMinutesSlept(fallAsleepTime: Date, awakeningTime: Date): List[Int] = {
    val dateFormat = new SimpleDateFormat("mm")
    (dateFormat.format(fallAsleepTime).toInt until dateFormat.format(awakeningTime).toInt).toList
  }

  private def assignShiftsToGuards(shifts: List[Shift]): List[Guard] = {
    shifts.groupBy(_.guardId)
      .map {case (guardId, shifts) => Guard(guardId, shifts)}
      .toList
  }

  /**
   * Strategy 1: Find the guard that was the most minutes asleep.
   * @param guards List of all guards
   * @return
   */
  private def firstStrategy(guards: List[Guard]): Option[Guard] = {
    val countTimeAsleep = guards.map(guard => (guard.guardId, guard.shifts.foldLeft(0)(_ + _.timeAsleep)))
    guards.find(_.guardId == countTimeAsleep.maxBy(_._2)._1)
  }

  /**
   * Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?
   *
   * @param guards List of all guards
   * @return
   */
  private def secondStrategy(guards: List[Guard]): Option[Guard] = {
    val countMinuteAsleep = guards.map(guard => (guard.guardId, getMostSleptMinute(guard.shifts)))
    guards.find(_.guardId == countMinuteAsleep.maxBy(_._2.times)._1)
  }

  private def getMostSleptMinute(shifts: List[Shift]): Minute = {
    val countMinutes = shifts.flatMap(_.minutesSlept).groupBy(minute => minute)
    if (countMinutes.isEmpty) Minute(-1, 0)
    else {
      val mostSleptMinute = countMinutes.maxBy(_._2.length)
      Minute(mostSleptMinute._1, mostSleptMinute._2.length)
    }
  }
}
