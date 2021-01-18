import scala.io.StdIn
import scala.collection.immutable.List

case class Nop ()
case class Acc (val value: Long)
case class Jmp (val value: Int)

object main {
    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        val reg = """(\w\w\w) ([+-]\d+)""".r
        val instructions = lines.map({
            case reg("nop", _) => Nop()
            case reg("acc", valueStr) => Acc(valueStr.toLong)
            case reg("jmp", valueStr) => Jmp(valueStr.toInt)
        })
        var acc = 0L
        var line = 0
        val visited = Array.fill(instructions.length)(false)
        var stop = false
        while (!stop) {
            if (line >= 0 && line < instructions.length && visited(line)) {
                stop = true
            } else if (line >= 0 && line < instructions.length) {
                visited(line) = true
                val instr = instructions(line)
                instr match {
                    case Nop() => line += 1
                    case Acc(v) => { acc += v; line += 1 }
                    case Jmp(v) => { line += v }
                }

            } else {
                throw new Error("out of bounds")
            }
        }
        println(instructions)
        println(acc)
    }
}
