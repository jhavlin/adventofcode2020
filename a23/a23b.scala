import scala.io.StdIn
import scala.collection.immutable.List

object main {
    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        var numbers = lines(0).split("").toList.map(_.toInt)
        numbers = numbers ++ ((numbers.max + 1) to 1000000)

        println(numbers.length)
        println(numbers.last)

        val next = Array.fill(numbers.length + 1)(0)
        var last = numbers(0)
        numbers.tail.foreach(n => {
            next(last) = n
            last = n
        })
        next(last) = numbers(0)



        var round = 1
        var currentValue = numbers(0)
        val l = numbers.length

        def state(): Unit = {
            (1 to l).foldLeft(currentValue)((last, _) => { print(s" $last "); next(last) })
            println()
        }

        def dec(i: Int): Int = {
            if (i > 1) {
                i - 1
            } else {
                l
            }
        }

        while (round <= 10000000) {
            // println(s"-- move $round --")
            // print("cups: ")
            // state()
            val (lastVal, toMove) = (1 to 3).foldLeft((currentValue, List[Int]()))({ case ((v, l), _) => { val n = next(v); (n, l :+ n) } })
            val after = next(lastVal)
            // println(s"pick up: ${toMove.mkString(", ")} ")
            var target = dec(currentValue)
            while (toMove.contains(target)) {
                target = dec(target)
            }
            val afterTarget = next(target)
            // println(s"destination: $target")
            // println(s"after: $after")

            next(currentValue) = after
            next(target) = toMove(0)
            next(toMove(2)) = afterTarget

            currentValue = next(currentValue)
            round += 1
        }

        val r1 = next(1)
        var r2 = next(next(1))

        println(s"$r1 * $r2 = ${r1.toLong * r2.toLong}")

         // println(s"-- final --")
         // state()
    }
}
