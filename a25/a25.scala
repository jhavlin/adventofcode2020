import scala.io.StdIn
import scala.collection.immutable.List

object main {

    def getLoopSize(key: Long, subjectNumber: Long): Int = {
        var value = 1L
        var round = 0;
        while (value != key) {
            value = (value * subjectNumber) % 20201227
            round += 1;
        }
        return round
    }

    def transform(subjectNumber: Long, loopSize: Int): Long = {
        var value = 1L;
        (1 to loopSize).foreach(_ => {
            value = (value * subjectNumber) % 20201227
        })
        return value
    }

    def main(args: Array[String]): Unit = {
        val publicKeys = io.Source.stdin.getLines().toList.map(_.toLong)
        println(publicKeys)

        val loopSize0 = getLoopSize(publicKeys(0), 7)
        val loopSize1 = getLoopSize(publicKeys(1), 7)

        println(transform(publicKeys(0), loopSize1))
        println(transform(publicKeys(1), loopSize0))
    }
}
