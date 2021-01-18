import scala.io.StdIn
import scala.collection.immutable.List

object main {
    val PREV = 25

    def isSum(index: Int, numbers: Array[Long]): Boolean = {
        val v = numbers(index)
        for (i <- (index - PREV) until index) {
            for (j <- (index - PREV) until i) {
                if (numbers(i) + numbers(j) == v) {
                    return true
                }
            }
        }
        false
    }

    def firstInvalid(numbers: Array[Long]): Long = {
        for (i <- PREV until numbers.length) {
            if (!isSum(i, numbers)) {
                return numbers(i)
            }
        }
        return -1;
    }

    def subSeq(sum: Long, numbers: Array[Long]): Unit = {
        val sums = Array.fill(numbers.length)(0L)
        sums(0) = numbers(0)
        for (i <- 1 until numbers.length) {
            sums(i) = numbers(i) + sums(i - 1)
        }
        for (i <- 0 until numbers.length) {
            for (j <- (i + 1) until numbers.length) {
                val prevSum = if (i == 0) 0 else sums(i - 1)
                val currentSum = sums(j) - prevSum
                if (currentSum == sum) {
                    println(i, j)
                    println(numbers(i), numbers(j))
                    val slice = numbers.slice(i, j + 1)
                    println(slice.min, slice.max)
                    println(slice.min + slice.max)
                    return
                }
            }
        }
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toArray
        val numbers = lines.map(str => str.toLong)
        
        val invalid = firstInvalid(numbers)
        println(invalid)
        println("----------")
        if (invalid > 0) {
            subSeq(invalid, numbers)
        }
    }
}
