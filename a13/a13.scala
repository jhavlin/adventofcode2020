import scala.io.StdIn
import scala.collection.immutable.List

object main {

    def lcm(number1: Long, number2: Long): Long =  {
        if (number1 == 0 || number2 == 0) {
            return 0;
        }
        val absNumber1 = Math.abs(number1);
        val absNumber2 = Math.abs(number2);
        val absHigherNumber = Math.max(absNumber1, absNumber2);
        val absLowerNumber = Math.min(absNumber1, absNumber2);
        var lcm = absHigherNumber;
        while (lcm % absLowerNumber != 0) {
            lcm = lcm + absHigherNumber;
        }
        lcm;
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        val arrival = lines(0).toLong
        val ids = lines(1).split(",").filter(_ != "x").map(_.toLong)
        val idsAndWaitTimes = ids.map(l => (l, l - arrival % l))
        val sorted = idsAndWaitTimes.sortBy(_._2).toList
        println(sorted)
        val best = sorted(0)
        println(best)
        println(best._1 * best._2)
        println("==========================")
        val idsAndOffsets = lines(1).split(",").toList.zipWithIndex.filter({ case (s, i) => s != "x" }).map({ case (s, i) => (s.toLong, i) })
        println(idsAndOffsets)
        val first = idsAndOffsets(0)
        var diff = first._1
        val start = first._1 - first._2
        println((start, diff))
        var found = false
        var curr = start
        while (!found) {
            if (idsAndOffsets.forall({ case (id, offset) => ((curr + offset) % id) == 0 })) {
                println(curr)
                found = true
            } else {
                idsAndOffsets.foreach({ case (id, offset) => {
                    if (((curr + offset) % id) == 0) {
                        diff = lcm(diff, id)
                    }
                }})
                curr += diff
            }
        }
    }
}
