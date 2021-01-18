import scala.io.StdIn
import scala.collection.immutable.List

object main {
    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        val passes = lines.map(toPass)
        passes.foreach(p => println(p))
        println("----")
        println(passes.max)
        val sorted = passes.sorted
        println("----")
        sorted.foreach(p => println(p))
        println("----")
        var prev = sorted(0) - 1
        sorted.foreach(p => {
            if (p != prev + 1) {
                println(p)
            }
            prev = p
        })
    }

    def toPass(line: String): Long = {
        val rowStr = line.substring(0, 7)
        val columnStr = line.substring(7, 10)
        val row = toBinary(rowStr, 'F', 'B')
        val column = toBinary(columnStr, 'L', 'R')
        row * 8 + column
    }

    def toBinary(str: String, char0: Char, char1: Char): Long = {
        str.foldLeft(0)((acc, curr) => {
            val plus = if (curr == char1) 1 else 0
            acc * 2 + plus
        });
    }
}
