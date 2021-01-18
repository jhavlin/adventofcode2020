import scala.io.StdIn
import scala.collection.immutable.List

case class Nop (val value: Int)
case class Acc (val value: Long)
case class Jmp (val value: Int)

object main {
    def terminates(instructions: List[Object]): Option[Long] = {
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
                    case Nop(_) => line += 1
                    case Acc(v) => { acc += v; line += 1 }
                    case Jmp(v) => { line += v }
                }
            } else if (line == instructions.length) {
                return Some(acc)
            } else {                
                throw new Error("out of bounds")
            }
        }
        None
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        val reg = """(\w\w\w) ([+-]\d+)""".r
        val instructions = lines.map({
            case reg("nop", valueStr) => Nop(valueStr.toInt)
            case reg("acc", valueStr) => Acc(valueStr.toLong)
            case reg("jmp", valueStr) => Jmp(valueStr.toInt)
        })
        instructions.zipWithIndex.find({ case (instr, index) =>
            instr match {
                case Nop(v) => {
                    val res = terminates(instructions.updated(index, Jmp(v)))
                    res match {
                        case Some(v) => { println(v); true }
                        case _ => false
                    }
                }
                case Jmp(v) => {
                    val res = terminates(instructions.updated(index, Nop(v)))
                    res match {
                        case Some(v) => { println(v); true }
                        case _ => false
                    }
                }
                case _ => false
            }
        })
    }
}
