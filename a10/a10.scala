import scala.io.StdIn
import scala.collection.immutable.List

object main {
    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        var numbers = lines.map(l => l.toLong).sorted
        val max = numbers(numbers.length - 1)
        numbers = 0 :: (numbers :+ (max + 3))
        var d1 = 0
        var d3 = 0
        println(numbers)
        for (i <- 1 until numbers.length) {
            if (numbers(i) == numbers(i - 1) + 1) {
                d1 += 1
            } else if (numbers(i) == numbers(i - 1) + 3) {
                d3 += 1
            } else {
                println("something else")
            }
        }
        println((d1, d3, d1 * d3))
        println("------")
        val len = numbers(numbers.length - 1).toInt
        val sums = Array.fill(len + 1)(0L)
        val present = Array.fill(len + 1)(false)
        numbers.foreach(num => { present(num.toInt) = true })
        for (i <- 1 to len) {
            if (present(i)) {
                val sum =
                    (if (i <= 3 && present(i)) 1 else 0) +
                    (if (i >= 1) sums(i - 1) else 0) +
                    (if (i >= 2) sums(i - 2) else 0) +
                    (if (i >= 3) sums(i - 3) else 0)
                sums(i) = sum
            }
        }
        println(present.toList)
        println(sums.toList)
        println(sums(sums.length - 1))
    }
}
