package utils

import scala.io.Source

object Utils {
  def readFile(path: String): List[String] = Source.fromResource(path).getLines.toList
}
