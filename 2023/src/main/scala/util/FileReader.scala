import scala.io.Source

object FileReader:
    def readFile(filename: String): Vector[String] =
        Source.fromResource(filename).getLines().toVector
