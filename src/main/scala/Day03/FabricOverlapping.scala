package Day03

import scala.annotation.tailrec
import scala.io.Source

case class Claim(id: String, x: Int, y: Int, width: Int, height: Int)

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

      Claim(id, position._1, position._2, measurements._1, measurements._2)
    })
  }

  private def getOverlappingSquares(claims: List[Claim]): Set[(Int, Int)] = {
    @tailrec
    def loop(claims: List[Claim], squares: Set[(Int, Int)], result: Set[(Int, Int)]): Set[(Int, Int)] = claims match {
      case Nil => result
      case claim :: rest =>
        val newSquares = for {
          x <- claim.x until claim.x + claim.width
          y <- claim.y until claim.y + claim.height
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
    val aSquares = for {
      x <- a.x until a.x + a.width
      y <- a.y until a.y + a.height
    } yield (x, y)
    val bSquares = for {
      x <- b.x until b.x + b.width
      y <- b.y until b.y + b.height
    } yield (x, y)

    aSquares.intersect(bSquares).nonEmpty
  }
}
