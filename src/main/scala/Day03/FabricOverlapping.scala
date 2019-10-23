package Day03

import scala.annotation.tailrec
import scala.io.Source

case class Claim(id: String, x1: Int, y1: Int, x2: Int, y2: Int)

object FabricOverlapping {
  def main(args: Array[String]): Unit = {
    val lines = readFile("src/main/scala/Day03/input.txt")
    val claims = convertToClaims(lines)
    val overlappingSquares = getOverlappingSquares(claims)
    val notOverlappingClaim = getNotOverlappingClaim(claims)
    println(overlappingSquares.size)
    println(notOverlappingClaim)
  }

  private def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    lines
  }

  private def convertToClaims(lines: List[String]): List[Claim] = {
    def extractValues(valueString: String, seperator: String): (Int, Int) = {
      val values = valueString.split(seperator)
      (values(0).toInt, values(1).toInt)
    }

    lines.map(line => {
      val idAndValues = line.split("@")
      val id = idAndValues(0).trim.substring(1)

      val values = idAndValues(1)
        .split(":")
        .map(_.trim)
      val position = extractValues(values(0), ",")
      val measurements = extractValues(values(1), "x")

      Claim(id, position._1, position._2, position._1 + measurements._1 - 1, position._2 + measurements._2 - 1)
    })
  }

  private def getOverlappingSquares(claims: List[Claim]): Set[(Int, Int)] = {
    @tailrec
    def loop(claims: List[Claim], squares: Set[(Int, Int)], result: Set[(Int, Int)]): Set[(Int, Int)] = claims match {
      case Nil => result
      case claim :: rest =>
        val newSquares = for {
          x <- claim.x1 to claim.x2
          y <- claim.y1 to claim.y2
        } yield (x, y)

        loop(rest, squares ++ newSquares, result ++ squares.intersect(newSquares.toSet))
    }

    loop(claims, Set(), Set())
  }

  private def getNotOverlappingClaim(allClaims: List[Claim]): Claim = {
    @tailrec
    def loop(claims: List[Claim]): Claim = claims match {
      case Nil => null
      case claim :: rest =>
        if (allClaims.forall(otherClaim =>
          if (claim == otherClaim) true else !areOverlapping(claim, otherClaim))
        ) claim
        else loop(rest)
    }

    loop(allClaims)
  }

  private def areOverlapping(a: Claim, b: Claim): Boolean = {
    val xRange = a.x1 to a.x2
    val yRange = a.y1 to a.y2
    val xInRange = (b.x1 to b.x2).exists(xRange.contains)
    val yInRange = (b.y1 to b.y2).exists(yRange.contains)

    xInRange && yInRange
  }
}
