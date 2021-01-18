import scala.io.StdIn
import scala.collection.immutable.List

object main {

    def stepsToCoords(s: String): (Int, Int) = {
        var row = 0
        var col = 0
        var last = ' ';
        s.foreach(c => {
            if (c == 'e') {
                if (last == ' ') { // e
                    col += 1
                } else if (last == 's') { // se
                    row -= 1
                    col += 1
                } else if (last == 'n') { // ne
                    row += 1
                } else {
                    throw new Error()
                }
                last = ' '
            } else if (c == 's') {
                last = c
            } else if (c == 'w') {
                if (last == ' ') { // w
                    col -= 1
                } else if (last == 's') { // sw
                    row -= 1
                } else if (last == 'n') { // nw
                    row += 1
                    col -= 1
                } else {
                    throw new Error()
                }
                last = ' '
            } else if (c == 'n') {
                last = c
            } else {
                throw new Error()
            }
        })
        (row, col)
    }

    def neighbors(coords: (Int, Int)): Set[(Int, Int)] = {
        val (row, col) = coords
        Set((row, col + 1), (row - 1, col + 1), (row - 1, col), (row, col - 1), (row + 1, col - 1), (row + 1, col))
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        var map = Map[(Int, Int), Boolean]()
        lines.foreach(l => {
            val coords = stepsToCoords(l)
            val current = map.getOrElse(coords, false)
            map = map.updated(coords, !current)
        })

        println("Result 1")
        println(map.values.filter(v => v).size)

        println(map)
        println()

        (1 to 100).foreach(day => {
            // println("map")
            // println(map)
            val relevant = map.keySet.flatMap(k => neighbors(k))
            // println("relevant")
            // println(relevant)
            val keysAndNewValues = relevant.map(coords => {
                val isBlack = map.getOrElse(coords, false)
                val countBlackAdjacent = neighbors(coords).toList.map(c => { val clr = map.getOrElse(c, false); clr }).filter(b => b).size
                // println(s"Tile at $coords has $countBlackAdjacent black neighbors")
                if (isBlack && (countBlackAdjacent == 0 || countBlackAdjacent > 2)) {
                    (coords, false)
                } else if (!isBlack && countBlackAdjacent == 2) {
                    (coords, true)
                } else {
                    (coords, isBlack)
                }
            })
            val newBlacks = keysAndNewValues.filter({ case (coords, color) => color })
            map = newBlacks.toMap
            println(s"$day: ${map.size}")
        })
    }
}
