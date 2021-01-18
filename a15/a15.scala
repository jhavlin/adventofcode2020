import scala.io.StdIn
import scala.collection.immutable.List

object main {

    def solve1(line: String, round: Long): Unit = {
        val starting = line.split(",").map(_.toLong).toList.reverse
        var list = starting
        for (i <- 1L to round) {
            print(s"$i: ")
            if (i <= starting.length) {
                // println(starting(list.length - i))
            } else {
                val last = list.head
                val index = list.tail.indexOf(last)
                if (index < 0) {
                    list = 0 +: list
                    println(0)
                } else {
                    list = (index + 1) +: list
                    println(index + 1)
                }
            }
        }
        println(list.head)
    }

    def solve2(line: String, round: Long): Unit = {
        val starting = line.split(",").map(_.toLong).toList
        var list = starting
        var map = Map[Long, List[Long]]()
        starting.zipWithIndex.foreach({ case (num, index) => map = map.updated(num, List(index.toLong + 1)) })
        var last: Long = starting.last
        var first = true
        for (i <- (starting.length + 1).toLong to round) {
            if (i % 1000000 == 0) {
                print(".")
            }
            if (first) {
                last = 0
                first = map.getOrElse(last, List()).length == 0
                map = map.updated(last, (i +: map.getOrElse(last, List())).take(2))
            } else {
                val indices = map.getOrElse(last, List())
                val diff = indices.head - indices.tail.head
                last = diff
                val seq = map.getOrElse(last, List())
                first = seq.length == 0
                map = map.updated(last, if (first) List(i) else List(i, seq.head))
            }
        }
        println()
        println(last)
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        lines.foreach(solve2(_, 2020))
        println("=======")
        lines.foreach(solve2(_, 30000000))
    }
}
