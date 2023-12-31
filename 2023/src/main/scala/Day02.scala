import cats.syntax.all.*

object Day02:
    private val input = FileReader.readFile("day02.txt")

    enum Cubes(val amount: Int, val max: Int):
        case Blue(a: Int)  extends Cubes(a, 14)
        case Red(a: Int)   extends Cubes(a, 12)
        case Green(a: Int) extends Cubes(a, 13)

        def isValid: Boolean = amount <= max
    end Cubes

    object Cubes:
        private def fromColorString(color: String, amount: Int) =
            color match
                case "blue"  => Cubes.Blue(amount).some
                case "red"   => Cubes.Red(amount).some
                case "green" => Cubes.Green(amount).some
                case _       => none
        end fromColorString

        def fromString(i: String): Option[Cubes] =
            val amtColTuple = i.trim match
                case s"$amount $color" => (amount, color).some
                case _                 => none

            for
                (aStr, cStr) <- amtColTuple
                a            <- aStr.toIntOption
                res          <- fromColorString(cStr, a)
            yield res
        end fromString
    end Cubes

    case class Round(cubes: Seq[Cubes])
    object Round:
        def fromString(i: String, enableMax: Boolean): Option[Round] =
            val res     = i.trim.split(',').toSeq.flatMap(Cubes.fromString)
            val isValid = !enableMax || res.forall(_.isValid)
            Option.when(isValid)(Round(res))
        end fromString

    end Round

    case class Game(id: Int, rounds: Seq[Round])
    object Game:
        def fromString(i: String, enableMax: Boolean): Option[Game] =
            val split = i.split(':').toSeq
            for
                h      <- split.headOption
                id     <- h.split(' ').lastOption
                iid    <- id.toIntOption
                l      <- split.lastOption
                rounds <- l.split(';').toSeq.traverse(Round.fromString(_, enableMax))
            yield Game(iid, rounds)

        end fromString
    end Game

    private def cubesNeeded(game: Game) =
        game.rounds
            .flatMap(_.cubes)
            .groupMapReduce(_.ordinal)(_.amount)((a, b) => math.max(a, b))
            .values
            .product

    private def part1() =
        val res = input.flatMap(Game.fromString(_, true))

        res.map(_.id).sum
    end part1

    private def part2() =
        input.flatMap(Game.fromString(_, false)).map(cubesNeeded).sum

    def run(): Unit =
        val res =
            s"""Part 1: ${part1()}
               |Part 2: ${part2()}""".stripMargin
        println(res)

end Day02
