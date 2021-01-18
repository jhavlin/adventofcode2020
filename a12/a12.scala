import scala.io.StdIn
import scala.collection.immutable.List

object main {

    case class Move(hor: Long, ver: Long)
    case class Rotate(r: Int)
    case class Forward(value: Long)

    val directions = List(Move(1, 0), Move(0, 1), Move(-1, 0), Move(0, -1))

    def rotate(dir: Move, count: Int): Move = {
        var pos = directions.indexOf(dir)
        var newPos = (4 + pos + count) % 4
        directions(newPos)
    }

    def solve1(instructions: List[Object]): Unit = {
        println(instructions)
        var dir = Move(1, 0)
        var pos = Move(0, 0)
        instructions.foreach(instr => {
            // print(instr)
            // print(": ")
            // print((pos, dir))
            // print(" => ")
            instr match {
                case Move(hor, ver) => { pos = Move(pos.hor + hor, pos.ver + ver) } 
                case Forward(value) => { pos = Move(pos.hor + dir.hor * value, pos.ver + dir.ver * value) }
                case Rotate(r) => { dir = rotate(dir, r) }
            }
            // print((pos, dir))
            // println()
        })
        println(pos)
        println(Math.abs(pos.hor) + Math.abs(pos.ver))
    }

    def rotateAround(pos: Move, count: Int): Move = {
        val moves = (4 + count) % 4
        var h = pos.hor
        var v = pos.ver
        for (_ <- 0 until moves) {
            val h2 = -v
            val v2 = h
            h = h2
            v = v2
        }
        Move(h, v)
    }

    def solve2(instructions: List[Object]): Unit = {
        var pos = Move(0, 0) // position
        var point = Move(10, -1) // waypoint
        instructions.foreach(instr => {
            instr match {
                case Move(hor, ver) => { point = Move(point.hor + hor, point.ver + ver) } 
                case Forward(value) => { pos = Move(pos.hor + point.hor * value, pos.ver + point.ver * value) }
                case Rotate(r) => { point = rotateAround(point, r) }
            }
        })
        println(pos)
        println(Math.abs(pos.hor) + Math.abs(pos.ver))
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        var regexp = """(\w)(\d+)""".r
        var instructions = lines.map({
            case regexp(c, v) => {
                c match {
                    case "N" => Move(0, -v.toLong)
                    case "S" => Move(0, v.toLong)
                    case "E" => Move(v.toLong, 0)
                    case "W" => Move(-v.toLong, 0)
                    case "L" => { if (!List("90", "180", "270").contains(v)) { throw new Error() }; Rotate(-1 * (v.toInt / 90)) } 
                    case "R" => { if (!List("90", "180", "270").contains(v)) { throw new Error }; Rotate(v.toInt / 90) }
                    case "F" => Forward(v.toLong)
                    case _ => throw new Error(s"Unsupported ${c} ${v}")
                }
            }
            case x => throw new Error(s"Unsupported ${x}")
        })
        // solve1(instructions)
        solve2(instructions)
    }
}
