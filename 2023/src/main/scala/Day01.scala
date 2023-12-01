import scala.annotation.tailrec
import cats.syntax.all.*

object Day01:
    private val input = FileReader.readFile("day01.txt")

    private def parseCalibrationValue(input: String) =
        val chars = input.toVector

        for
            first <- chars.find(_.isDigit)
            last  <- chars.findLast(_.isDigit)
        yield s"$first$last".toInt
    end parseCalibrationValue

    private def part1() =
        val res = input.flatMap(parseCalibrationValue).sum

        res
    end part1

    private def parseCalibrationValuePart2(str: String) =
        @tailrec
        def mapToDigits(values: List[Char], acc: List[Int]): List[Int] =
            values match
                case Nil                                  => acc
                case h :: tail if h.isDigit               => mapToDigits(tail, acc :+ h.asDigit)
                case 'o' :: 'n' :: 'e' :: _               => mapToDigits(values.tail, acc :+ 1)
                case 't' :: 'w' :: 'o' :: _               => mapToDigits(values.tail, acc :+ 2)
                case 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ => mapToDigits(values.tail, acc :+ 3)
                case 'f' :: 'o' :: 'u' :: 'r' :: _        => mapToDigits(values.tail, acc :+ 4)
                case 'f' :: 'i' :: 'v' :: 'e' :: _        => mapToDigits(values.tail, acc :+ 5)
                case 's' :: 'i' :: 'x' :: _               => mapToDigits(values.tail, acc :+ 6)
                case 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ => mapToDigits(values.tail, acc :+ 7)
                case 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ => mapToDigits(values.tail, acc :+ 8)
                case 'n' :: 'i' :: 'n' :: 'e' :: _        => mapToDigits(values.tail, acc :+ 9)
                case _                                    => mapToDigits(values.tail, acc)

        val digits = mapToDigits(str.toList, Nil)

        (digits.headOption, digits.lastOption).mapN((h, l) => s"$h$l".toInt)
    end parseCalibrationValuePart2

    private def part2() =
        input.flatMap(parseCalibrationValuePart2).sum
    end part2

    def run(): Unit =
        val res1 = part1()
        val res2 = part2()

        val output =
            s"""Part 1 = $res1
               |Part 2 = $res2""".stripMargin

        println(output)
    end run
end Day01
