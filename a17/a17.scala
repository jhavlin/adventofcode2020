import scala.io.StdIn
import scala.collection.immutable.List

object main {
    
    def maximum(v: Long*): Long = v.max
    def minimum(v: Long*): Long = v.min

    def neighbors(set: Set[(Long, Long, Long)], x: Long, y: Long, z: Long): Int = {
        var count = 0
        for (dx <- -1L to 1L) {
            for (dy <- -1L to 1L) {
                for (dz <- -1L to 1L) {
                    if (dx != 0 || dy != 0 || dz != 0) {
                        if (set.contains((x + dx, y + dy, z + dz))) {
                            count += 1
                        }
                    }
                }
            }
        }
        count
    }

    def neighbors4d(set: Set[(Long, Long, Long, Long)], x: Long, y: Long, z: Long, a: Long): Int = {
        var count = 0
        for (dx <- -1L to 1L) {
            for (dy <- -1L to 1L) {
                for (dz <- -1L to 1L) {
                    for (da <- -1L to 1L) {
                        if (dx != 0 || dy != 0 || dz != 0 || da != 0) {
                            if (set.contains((x + dx, y + dy, z + dz, a + da))) {
                                count += 1
                            }
                        }
                    }
                }
            }
        }
        count
    }

    def solve1(lines: List[String]): Unit = {
        var set = Set[(Long, Long, Long)]()
        var max = 0L;
        var min = 0L;
        lines.zipWithIndex.foreach({
            case (row, x) => {
                row.zipWithIndex.foreach({
                    case (char, y) => {
                        if (char == '#') {
                            set = set + ((x.toLong, y.toLong, 0L))
                        }
                        max = maximum(max, x, y)
                        min = minimum(min, x, y)
                    }
                })
            }
        })
        for (round <- 1 to 6) {
            var newSet = Set[(Long, Long, Long)]()
            var nextMax = 0L
            var nextMin = 0L
            for (x <- (min - 1) to (max + 1)) {
                for (y <- (min - 1) to (max + 1)) {
                    for (z <- (min - 1) to (max + 1)) {
                        val active = set.contains((x, y, z))
                        val near = neighbors(set, x, y, z)
                        if (active && (near == 2 || near == 3)) {
                            newSet = newSet + ((x, y, z))
                        } else if (!active && near == 3) {
                            newSet = newSet + ((x, y, z))
                        }
                        nextMax = maximum(nextMax, x, y, z)
                        nextMin = minimum(nextMin, x, y, z)
                        // println(round, x, y, z)
                    } 
                }
            }
            set = newSet
            max = nextMax
            min = nextMin
        }
        println(set.size)
    }

    def solve2(lines: List[String]): Unit = {
        var set = Set[(Long, Long, Long, Long)]()
        var max = 0L;
        var min = 0L;
        lines.zipWithIndex.foreach({
            case (row, x) => {
                row.zipWithIndex.foreach({
                    case (char, y) => {
                        if (char == '#') {
                            set = set + ((x.toLong, y.toLong, 0L, 0L))
                        }
                        max = maximum(max, x, y)
                        min = minimum(min, x, y)
                    }
                })
            }
        })
        for (round <- 1 to 6) {
            var newSet = Set[(Long, Long, Long, Long)]()
            var nextMax = 0L
            var nextMin = 0L
            for (x <- (min - 1) to (max + 1)) {
                for (y <- (min - 1) to (max + 1)) {
                    for (z <- (min - 1) to (max + 1)) {
                        for (a <- (min - 1) to (max + 1)) {
                            val active = set.contains((x, y, z, a))
                            val near = neighbors4d(set, x, y, z, a)
                            if (active && (near == 2 || near == 3)) {
                                newSet = newSet + ((x, y, z, a))
                            } else if (!active && near == 3) {
                                newSet = newSet + ((x, y, z, a))
                            }
                            nextMax = maximum(nextMax, x, y, z, a)
                            nextMin = minimum(nextMin, x, y, z, a)
                            // println(round, x, y, z)
                        }
                    } 
                }
            }
            set = newSet
            max = nextMax
            min = nextMin
        }
        println(set.size)
    }


    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        solve1(lines)
        solve2(lines)
    }
}
