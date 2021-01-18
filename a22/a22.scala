import scala.io.StdIn
import scala.collection.immutable.List
import scala.collection.immutable.Queue


object main {

    def solve1(oq1: Queue[Int], oq2: Queue[Int]): Unit = {
        var q1 = oq1
        var q2 = oq2
        println(q1)
        println(q2)
        while (q1.size > 0 && q2.size > 0) {
            val (v1, newQ1) = q1.dequeue
            val (v2, newQ2) = q2.dequeue
            if (v1 > v2) {
                q1 = newQ1 :+ v1 :+ v2
                q2 = newQ2
            } else {
                q1 = newQ1
                q2 = newQ2 :+ v2 :+ v1
            }
        }
        println(q1)
        println(q2)

        val winner = if (q1.size > 0) q1 else q2

        var result = winner.toList.reverse.zipWithIndex.map({ case (v, i) => v * (i + 1) }).sum
        println(result)
    }

    def subgame(oq1: Queue[Int], oq2: Queue[Int]): (Int, Queue[Int], Queue[Int]) = {
        var q1 = oq1
        var q2 = oq2
        var used = Set[(List[Int], List[Int])]()

        while (q1.size > 0 && q2.size > 0 && !used.contains((q1.toList, q2.toList))) {
            used = used + ((q1.toList, q2.toList))
            val (v1, newQ1) = q1.dequeue
            val (v2, newQ2) = q2.dequeue
            if (v1 <= newQ1.size && v2 <= newQ2.size) {
                val (winner, _,  _) = subgame(newQ1.take(v1), newQ2.take(v2))
                if (winner == 1) {
                    q1 = newQ1 :+ v1 :+ v2
                    q2 = newQ2
                } else if (winner == 2) {
                    q1 = newQ1
                    q2 = newQ2 :+ v2 :+ v1
                } else {
                    throw new Error(s"Invalid winner $winner")
                }
            } else {
                if (v1 > v2) {
                    q1 = newQ1 :+ v1 :+ v2
                    q2 = newQ2
                } else {
                    q1 = newQ1
                    q2 = newQ2 :+ v2 :+ v1
                }
            }
        }
        if (q1.size > 0 && q2.size > 0 && used.contains((q1.toList, q2.toList))) {
            (1, q1, q2)
        } else if (q1.size == 0 || q2.size == 0) {
            val winner = if (q1.size > 0) 1 else 2
            (winner, q1, q2)
        } else {
            throw new Error(s"Invalid result: q1 = ${q1}, q2 = ${q2}, used = ${used}")
        }
    }

    def solve2(oq1: Queue[Int], oq2: Queue[Int]): Unit = {
        val (winner, q1, q2) = subgame(oq1, oq2)
        val winnerQueue = if (winner == 1) {
            q1
        } else if (winner == 2) {
            q2
        } else {
            throw new Error(s"Invalid end winner $winner")
        }
        val result: Long = winnerQueue.toList.reverse.zipWithIndex.map({ case (v, i) => v.toLong * (i + 1) }).sum
        println(result);
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        var q1 = Queue[Int]()
        var q2 = Queue[Int]()
        var mode = 0
        lines.foreach(l => {
            if (l.isEmpty) {
                // no op
            } else if (l == "Player 1:") {
                mode = 1
            } else if (l == "Player 2:") {
                mode = 2
            } else {
                if (mode == 1) {
                    q1 = q1 :+ l.toInt
                } else if (mode == 2) {
                    q2 = q2 :+ l.toInt
                }
            }
        })
        
        solve1(q1, q2)
        println("=========")
        solve2(q1, q2)
    }
}
