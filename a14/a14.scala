import scala.io.StdIn
import scala.collection.immutable.List

object main {

    case class Instr(address: Int, value: Long)

    val maskRE = """mask = (.{36})""".r
    val instrRE = """mem\[(\d+)\] = (\d+)""".r

    def mask(maskStr: String, value: Long): Long = {
        var curr = value
        var i = 35
        var bitValue = 1L
        var res = 0L
        while (i >= 0) {
            val bit = curr % 2
            curr = curr / 2
            val maskChar = maskStr.charAt(i)
            val newBit = maskChar match {
                case 'X' => bit
                case '0' => 0
                case '1' => 1
                case o => throw new Error(s"Unsupported char ${o}")
            }
            res = res + (if (newBit == 1) bitValue else 0)
            i -= 1
            bitValue *= 2
        }
        // println("Converting")
        // println(value.toBinaryString + " to")
        // println(res.toBinaryString)
        res
    }

    def solve1(lines: List[String]): Unit = {
        var maskStr = "";
        var memory = Map[Int, Long]()
        lines.foreach({
            case maskRE(s) => { maskStr = s }
            case instrRE(a, v) => { memory = memory.updated(a.toInt, mask(maskStr, v.toLong)) }
        })
        println(memory)
        println(memory.values.sum)
    }

    def mask2(maskStr: String, value: Long): List[Long] = {
        var curr = value
        var i = 35
        var bitValue = 1L
        var results  = List[Long](0L)
        while (i >= 0) {
            val bit = curr % 2
            curr = curr / 2
            val maskChar = maskStr.charAt(i)
            maskChar match {
                case '0' => {
                    results = results.map(res => res + (if (bit == 1) bitValue else 0))
                }
                case '1' => {
                    results = results.map(res => res + bitValue)
                }
                case 'X' => {
                    results = results.flatMap(res => List(res, res + bitValue))
                }
                case o => throw new Error(s"Unsupported char ${o}")
            }
            i -= 1
            bitValue *= 2
        }
        results
    }

    def solve2(lines: List[String]): Unit = {
        var maskStr = "";
        var memory = Map[Long, Long]()
        lines.foreach({
            case maskRE(s) => { maskStr = s }
            case instrRE(a, v) => {
                val addresses = mask2(maskStr, a.toLong)
                addresses.foreach(address => {
                    memory = memory.updated(address, v.toLong)
                })
            }
        })
        println(memory)
        println(memory.values.sum)
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        solve1(lines);
        println("===========")
        solve2(lines);
    }
}
