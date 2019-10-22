package Day03

import scala.annotation.tailrec
import scala.io.Source

case class Claim(x: Int, y: Int, width: Int, height: Int)

object FabricOverlapping {
  def main(args: Array[String]): Unit = {
    val lines = readFile("src/main/scala/Day03/input.txt")
    val claims = convertToClaims(lines)
    println(getOverlappingSquares(claims).size)
  }

  private def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    lines
  }

  private def convertToClaims(lines: List[String]): List[Claim] = {
    def extractValues(line: String, seperator: String): (Int, Int) = {
      val values = line.split(seperator)
      (values(0).toInt, values(1).toInt)
    }

    lines.map(line => {
      val data = line.dropWhile(c => c != '@')
        .substring(1)
        .split(":")
        .map(_.trim)
      val position = extractValues(data(0), ",")
      val measurements = extractValues(data(1), "x")

      Claim(position._1, position._2, measurements._1, measurements._2)
    })
  }

  private def getOverlappingSquares(claims: List[Claim]): Set[(Int, Int)] = {
    @tailrec
    def loop(claims: List[Claim], squares: Set[(Int, Int)], result: Set[(Int, Int)]): Set[(Int, Int)] = {
      if (claims.isEmpty) result
      else {
        val claim = claims.head
        val newSquares = for {
          x <- claim.x until claim.x + claim.width
          y <- claim.y until claim.y + claim.height
        } yield (x, y)

        loop(claims.tail, squares ++ newSquares, result ++ squares.intersect(newSquares.toSet))
      }
    }

    loop(claims, Set(), Set())
  }
}
