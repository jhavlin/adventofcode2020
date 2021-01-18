import scala.io.StdIn
import scala.collection.immutable.List

object main {

    type Seats =  Array[Array[Char]]

    def printLines(lines: Seats): Unit = {
        lines.foreach(line => {
            line.foreach(c => print(c))
            println("")
        })
    }

    def countAdjacentOccupied(lines: Seats, line: Int, column: Int): Int = {
        var count = 0
        for (l <- (line - 1) to (line + 1)) {
            if (l >= 0 && l < lines.length) {
                val row = lines(l)
                for (c <- (column - 1) to (column + 1)) {
                    if (c >= 0 && c < row.length && !(c == column && l == line)) {
                        val char = row(c)
                        if (char == '#') {
                            count += 1
                        }
                    }
                }
            }
        }
        if (line == 1 && column == 0) {
            println("Count 1 0 " + count)
        }
        count
    }

    def reduceSeats[A](seats: Seats, startValue: A, fn: (A, Char, Int, Int) => A): A = {
        var acc = startValue
        for (l <- 0 until seats.length) {
            val line = seats(l)
            for (c <- 0 until line.length) {
                val char = line(c)
                acc = fn(acc, char, l, c)
            }
        }
        acc
    }

    def main1(lines: List[String]) {
        var seats1 = lines.map(s => s.toArray).toArray
        var seats2 = lines.map(s => s.toArray).toArray
        printLines(seats1)
        var changed = false
        var round = 1
        do {
            changed = false
            reduceSeats(seats1, (), (_:Unit, char, line, column) => {
                val occupied = countAdjacentOccupied(seats1, line, column)
                if (char == 'L' && occupied == 0) {
                    seats2(line)(column) = '#';
                    changed = true
                } else if (char == '#' && occupied >= 4) {
                    seats2(line)(column) = 'L';
                    changed = true
                } else {
                    seats2(line)(column) = char;
                }
            })
            val temp: Seats = seats1
            seats1 = seats2
            seats2 = temp
            println()
            println("Round: " + round)
            round += 1
            println("=================================")
            printLines(seats1)
        } while (changed);
        println(reduceSeats(seats1, 0, (count: Int, char, _, _) => if (char == '#') count + 1 else count))
    }

    def countVisibleOccupied(seats: Seats, line: Int, column: Int): Int = {
        val directions = (-1 to 1).flatMap(r => (-1 to 1).map(c => (r, c))).toList.filter({ case (r, c) => r != 0 || c != 0 })
        def seeOccupidInDirection(dr: Int, dc: Int, r: Int, c: Int): Int = {
            val ar = dr + r; // actual row
            val ac = dc + c; // actual column
            if (ar >= 0 && ar < seats.length && ac >= 0 && ac < seats(ar).length) {
                val char = seats(ar)(ac)
                if (char == 'L') {
                    0
                } else if (char == '#') {
                    1
                } else {
                    seeOccupidInDirection(dr, dc, ar, ac)
                }
            } else
                0
        }
        directions.map({ case (dr, dc) => seeOccupidInDirection(dr, dc, line, column) }).sum
    }

    def main2(lines: List[String]) {
        var seats1 = lines.map(s => s.toArray).toArray
        var seats2 = lines.map(s => s.toArray).toArray
        printLines(seats1)
        var changed = false
        var round = 1
        do {
            changed = false
            reduceSeats(seats1, (), (_:Unit, char, line, column) => {
                val occupied = countVisibleOccupied(seats1, line, column)
                if (char == 'L' && occupied == 0) {
                    seats2(line)(column) = '#';
                    changed = true
                } else if (char == '#' && occupied >= 5) {
                    seats2(line)(column) = 'L';
                    changed = true
                } else {
                    seats2(line)(column) = char;
                }
            })
            val temp: Seats = seats1
            seats1 = seats2
            seats2 = temp
            println()
            println("Round: " + round)
            round += 1
            println("=================================")
            printLines(seats1)
        } while (changed);
        println(reduceSeats(seats1, 0, (count: Int, char, _, _) => if (char == '#') count + 1 else count))
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        // main1(lines);
        main2(lines);
    }
}
