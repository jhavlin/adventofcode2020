import scala.io.StdIn
import scala.collection.immutable.List

object main {
    def count(lines: List[String], right: Int, down: Int): Long = {
        var col = right;
        var sum = 0
        lines.tail.zipWithIndex.foreach({ case (line, index) => {
            if (down == 1 || index % down == 1 ) {
                val c = line.charAt(col % line.length)
                if (c == '#') {
                    sum += 1
                }
                col += right
            }
        }})
        return sum
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        val r = count(lines, 3, 1)
        println(r)
        println("------")
        val r1 = count(lines, 1, 1)
        val r3 = count(lines, 3, 1)
        val r5 = count(lines, 5, 1)
        val r7 = count(lines, 7, 1)
        val r12 = count(lines, 1, 2)
        println((r1, r3, r5, r7, r12, "=", r1 * r3 * r5 * r7 * r12))
    }
}
