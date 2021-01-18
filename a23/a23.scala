import scala.io.StdIn
import scala.collection.immutable.List

object main {
    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        var numbers = lines(0).split("").toList.map(_.toInt)
        println(numbers.toList)
        var round = 1
        var currentIndex = 0
        val l = numbers.length
        println(numbers.mkString(" "))
        while (round <= 100L) {
            println(s"-- move $round --")
            print("cups: ")
            numbers.zipWithIndex.foreach({ case (v, index) => print(if (index == currentIndex) s"($v)" else s" $v ") })
            println()
            val list = numbers ++ numbers
            val (beforeCurrent, afterCurrent) = list.splitAt(currentIndex)
            val toMove = afterCurrent.tail.take(3)
            println(s"pick up: ${toMove.mkString(", ")} ")
            val current = list(currentIndex)
            val rest = afterCurrent.drop(4).take(5)

            var max = 0
            var maxIndex = -1
            var min = l + 1
            var minIndex = -1
            rest.zipWithIndex.foreach({ case (v, i) => {
                if (v < current && (v > min || min == l + 1)) {
                    min = v
                    minIndex = i
                }
                if (v > max) {
                    max = v
                    maxIndex = i
                }
            } })
            println(s"  $rest, min = $min ($minIndex), max = $max ($maxIndex), current = $current")
            val indexInRest = if (minIndex > -1) minIndex else maxIndex
            println(s"destination: ${rest(indexInRest)}")
            val (restStart, restEnd) = rest.splitAt(indexInRest + 1)
            val newRest = current +: (restStart ++ toMove ++ restEnd)

            val (firstHalf, secondHalf) = newRest.splitAt(l - currentIndex)
            numbers = secondHalf ++ firstHalf

            // 0 1 2 3 4 5
            // 5 0 1 2 3 4
            // 1 2 3 4 5 0

            // println(beforeCurrent, current, toMove, rest, newRest)
            currentIndex = (currentIndex + 1) % l
            round += 1
            println()
        }

        println(s"-- final --")
        print("cups: ")
        numbers.zipWithIndex.foreach({ case (v, index) => print(if (index == currentIndex) s"($v)" else s" $v ") })
        println()

        val doubled = numbers ++ numbers
        println(doubled.drop(doubled.indexOf(1) + 1).take(l - 1).mkString(""))
    }
}
