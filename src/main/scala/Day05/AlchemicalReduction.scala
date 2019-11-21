package Day05

import scala.annotation.tailrec
import utils.Utils.readFile

object AlchemicalReduction extends App {
  val polymer = readFile("Day05/input.txt").head
  val reducedPolymer = reducePolymer(polymer)
  val improvedPolymers = getImprovedPolymers(polymer)
  val optimalPolymer = findOptimalPolymer(improvedPolymers)

  // Results
  println(s"Part 01: ${reducedPolymer.length}")
  println(s"Part 02: ${optimalPolymer.length}")

  private def getImprovedPolymers(polymer: String): List[String] =
    (for {
      unitToRemove <- 'a' to 'z'
    } yield reducePolymer(removeUnit(polymer, unitToRemove))).toList

  private def reducePolymer(polymer: String): String = {
    @tailrec
    def loop(reducedPolymer: String, reduced: Boolean, index: Int): String = {
      if (index >= reducedPolymer.length - 2)
        if (reduced)
          loop(reducedPolymer, reduced = false, 0)
        else
          reducedPolymer
      else {
        val unitA = reducedPolymer(index)
        val unitB = reducedPolymer(index +  1)
        if (isSameType(unitA, unitB) && isOppositePolarity(unitA, unitB))
          loop(reducedPolymer.take(index) + reducedPolymer.drop(index + 2), reduced = true, index + 1)
        else
          loop(reducedPolymer, reduced, index + 1)
      }
    }

    loop(polymer, reduced = false, 0)
  }

  private def removeUnit(polymer: String, unitToRemove: Char): String =
    polymer.filter(unit => unit != unitToRemove.toUpper && unit != unitToRemove.toLower)

  private def findOptimalPolymer(polymers: List[String]): String =
    polymers.minBy(_.length)

  private def isSameType(unitA: Char, unitB: Char): Boolean =
    unitA.toUpper == unitB.toUpper

  private def isOppositePolarity(unitA: Char, unitB: Char): Boolean =
    unitA.isLower && unitB.isUpper || unitA.isUpper && unitB.isLower
}
