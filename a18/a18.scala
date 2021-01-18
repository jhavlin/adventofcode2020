import scala.io.StdIn
import scala.collection.immutable.List

object main {

    def evalAt1(line: String, pos: Int): (Long, Int) = {
        var value: Long = 0
        var hasCurrent = false
        var current: Long = 0
        var op = ' ';
        var p = pos
        def compute(): Unit = {
            if (op == '+' && hasCurrent) {
                // println(s"$value + $current = ${value + current}")
                value = value + current
                op = ' '
                hasCurrent = false;
                current = 0
            } else if (op == '*' && hasCurrent) {
                // println(s"$value * $current = ${value * current}")
                value = value * current
                op = ' '
                hasCurrent = false
                current = 0
            } else if (hasCurrent) {
                // println(s"value = current: $current")
                value = current
                current = 0
                hasCurrent = false
            }
        }
        while (p < line.length) {
            val c = line.charAt(p)
            // println(s"char ${c}")
            if (c.isDigit) {
                current = (current * 10) + c.toString.toInt
                hasCurrent = true
            } else if (c == '(') {
                val (nc, np) = evalAt1(line, p + 1)
                current = nc
                hasCurrent = true
                p = np
                compute()
            } else if (c == ')') {
                compute()
                return (value, p)
            } else if (c == ' ') {
                compute()
            } else if (c == '+') {
                op = c
            } else if (c == '*') {
                op = c
            } else {
                throw new Error(s"Unexpected char ${c}")
            }
            p += 1
        }
        compute()
        (value, p)
    }

    def eval1(line: String): Long = {
        evalAt1(line, 0)._1
    }

    def solve1(lines: List[String]): Unit = {
        println(lines.map(eval1).sum)
    }

    def eval2(line: String): Long = {
        evalAt2(line, 0)._1
    }

    def evalAt2(line: String, pos: Int): (Long, Int) = {
        var value: List[Long] = List()
        var hasCurrent = false
        var current: Long = 0
        var op = ' ';
        var p = pos
        def compute(): Unit = {
            if (op == '+' && hasCurrent) {
                // println(s"$value + $current = ${value + current}")
                value = (value :+ -2L) :+ current
                op = ' '
                hasCurrent = false;
                current = 0
            } else if (op == '*' && hasCurrent) {
                // println(s"$value * $current = ${value * current}")
                value = (value :+ -1L) :+ current
                op = ' '
                hasCurrent = false
                current = 0
            } else if (hasCurrent) {
                // println(s"value = current: $current")
                value = value :+ current
                current = 0
                hasCurrent = false
            }
        }
        def eval(v: List[Long]): Long = {
            val multiplications = v.iterator.foldLeft(List(0L))((acc, curr) => {
                if (curr == -2) {
                    acc
                } else if (curr == -1) {
                    0L +: acc
                } else {
                    (acc.head + curr) +: acc.tail
                }
            })
            // println(s"$v => $multiplications -> ${multiplications.product}")
            multiplications.product
        }
        while (p < line.length) {
            val c = line.charAt(p)
            // println(s"char ${c}")
            if (c.isDigit) {
                current = (current * 10) + c.toString.toInt
                hasCurrent = true
            } else if (c == '(') {
                val (nc, np) = evalAt2(line, p + 1)
                current = nc
                hasCurrent = true
                p = np
                compute()
            } else if (c == ')') {
                compute()
                return (eval(value), p)
            } else if (c == ' ') {
                compute()
            } else if (c == '+') {
                op = c
            } else if (c == '*') {
                op = c
            } else {
                throw new Error(s"Unexpected char ${c}")
            }
            p += 1
        }
        compute()
        (eval(value), p)
    }

    def solve2(lines: List[String]): Unit = {
        println(lines.map(eval2).sum)
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        // solve1(lines)
        solve2(lines)
    }
}
