package Day05

import scala.annotation.tailrec
import utils.Utils.readFile

object AlchemicalReduction extends App {
  val line = readFile("Day05/input.txt").head
  val reducedPolymer = reducePolymer(line)

  // Result
  println(s"Part 01: ${reducedPolymer.length}")

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

  private def isSameType(unitA: Char, unitB: Char): Boolean = unitA.toUpper == unitB.toUpper

  private def isOppositePolarity(unitA: Char, unitB: Char): Boolean = unitA.isLower && unitB.isUpper || unitA.isUpper && unitB.isLower
}
