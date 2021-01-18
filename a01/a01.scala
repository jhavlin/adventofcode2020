import scala.io.StdIn
import scala.collection.immutable.List

object main {
    def main(args: Array[String]): Unit = {
        var l = List[Int]()
        for (ln <- io.Source.stdin.getLines()) {
            var n = ln.toInt
            l = l :+ n
        }
        for (i <- 0 until l.length) {
            for (j <- 0 until i) {
                if (i != j && l(i) + l(j) == 2020) {
                    println(l(i), l(j), l(i) * l(j))
                }
            }
        }
        for (i <- 0 until l.length) {
            for (j <- 0 until i) {
                for (k <- 0 until j) {
                    if (l(i) + l(j) + l(k) == 2020) {
                        println(l(i), l(j), l(k), l(i) * l(j) * l(k))
                    }
                }
            }
        }
    }
}
