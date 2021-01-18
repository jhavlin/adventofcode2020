import scala.io.StdIn
import scala.collection.immutable.List

object main {
    val labelR = """Tile (\d+):""".r

    val TOP = 0
    val RIGHT = 1
    val BOTTOM = 2
    val LEFT = 3

    def variants(rows: Array[String]): Set[Array[String]] = {
        def rotate(rows: Array[String]): Array[String] = {
            val res = Array.fill(rows(0).length)("")
            for(i <- 0 until res.length) {
                res(i) = rows.map(r => r.charAt(i)).mkString("")
            }
            res
        }
        def flipVertical(rows: Array[String]): Array[String] = {
            rows.toList.reverse.toArray
        }
        def flipHorizontal(rows: Array[String]): Array[String] = {
            rows.map(r => r.reverse)
        }
        val rotations = (1 to 3).foldLeft(List(rows))((acc, curr) => rotate(acc.head) +: acc).toSet
        val as = rotations.flatMap(v => Set(v, flipVertical(v), flipHorizontal(v), flipVertical(flipHorizontal(v))))
        val unique = as.toList.map(a => a.mkString("\n")).toSet.map((s: String) => s.split("\n").toArray)
        unique
    }

    class Tile(val id: Long, val lines: Array[String]) {

        def this(lines: List[String]) = this(lines.head match { case labelR(idStr) => idStr.toLong }, lines.tail.toArray)

        val edgeStrings: Array[String] = getEdgeStrings(lines)
        val edgeValueList = edgeStrings.map(toVal(_))
        val edgeValues = edgeStrings.toSet.flatMap(edgeToValues(_))

        def getEdgeStrings(lines: Array[String]): Array[String] = {
            val top = lines(0)
            val bottom = lines(lines.length - 1)
            val left = lines.map(l => l.charAt(0)).mkString("")
            val right = lines.map(l => l.charAt(l.length - 1)).mkString("")
            Array(top, right, bottom, left)
        }

        def toVal(s: String): Long = {
            var v: Long = 0
            s.foreach(c => { v = v * 2 + (if (c == '#') 1 else 0 ) })
            v
        }

        def edgeToValues(edge: String): Set[Long] = {
            Set(toVal(edge), toVal(edge.reverse))
        }

        def intersectsWith(t: Tile): Boolean = {
            t.edgeValues.intersect(this.edgeValues).size > 0
        }

        def allVariants: Set[Tile] = {
            variants(lines).map(variant => new Tile(id, variant))
        }

        def getVariantForContext(top: Tile, right: Tile, bottom: Tile, left: Tile): Tile = {
            allVariants.find(v => v.isValidInContext(top, right, bottom, left)).get
        }

        def isValidInContext(top: Tile, right: Tile, bottom: Tile, left: Tile): Boolean = {
            (top == null || top.edgeValues.contains(edgeValueList(TOP))) &&
            (right == null || right.edgeValues.contains(edgeValueList(RIGHT))) &&
            (bottom == null || bottom.edgeValues.contains(edgeValueList(BOTTOM))) &&
            (left == null || left.edgeValues.contains(edgeValueList(LEFT)))
        }

        override def toString(): String = {
            val full = s"${edgeStrings(TOP)}, ${edgeStrings(RIGHT)}, ${edgeStrings(BOTTOM)}, ${edgeStrings(LEFT)}"
            s"$id: $full $edgeValues"
        }
    }

    def countAdjacent(tile: Tile, tiles: List[Tile]): Int = {
        tiles.filter(t => t != tile &&  tile.edgeValues.intersect(t.edgeValues).size > 0).length
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        val tilesStrs = lines.mkString("\n").split("\n\n").toList
        val tiles = tilesStrs.map(str => new Tile(str.split("\n").toList))
        // tiles.foreach(println(_))
        val edgeTiles = tiles.filter(t => countAdjacent(t, tiles) == 2)
        println("Edge tiles:")
        edgeTiles.foreach(println(_))
        val edgesProduct = edgeTiles.map(_.id).product
        println(edgesProduct)

        val a = Math.round(Math.sqrt(tiles.length)).toInt
        println(s"a = $a")

        val grouped = tiles.groupBy(t => countAdjacent(t, tiles))
        val matrix: Array[Array[Tile]] = Array.fill(a)(Array.fill(a)(null))
        matrix(0)(0) = edgeTiles(0)
        var used = Set(edgeTiles(0))
        val l = tiles.length
        
        var current = edgeTiles(0)

        // top edge
        for (c <- 1 until (a - 1)) {
            val next = grouped(3).find(t => t.intersectsWith(current) && !used.contains(t)).get
            matrix(0)(c) = next
            used = used + next
            current = next
        }
        // top right corner
        {
            val next = grouped(2).find(t => t.intersectsWith(current) && !used.contains(t)).get
            matrix(0)(a - 1) = next
            used = used + next
            current = next
        }
        // right edge
        for (r <- 1 until (a - 1)) {
            val next = grouped(3).find(t => t.intersectsWith(current) && !used.contains(t)).get
            matrix(r)(a - 1) = next
            used = used + next
            current = next
        }
        // bottom right corner
        {
            val next = grouped(2).find(t => t.intersectsWith(current) && !used.contains(t)).get
            matrix(a - 1)(a - 1) = next
            used = used + next
            current = next
        }
        // bottom edge
        for (c <- 1 until (a - 1)) {
            val next = grouped(3).find(t => t.intersectsWith(current) && !used.contains(t)).get
            matrix(a - 1)(a - 1 - c) = next
            used = used + next
            current = next
        }
        // bottom left corner
        {
            val next = grouped(2).find(t => t.intersectsWith(current) && !used.contains(t)).get
            matrix(a - 1)(0) = next
            used = used + next
            current = next
        }
        // left edge
        for (r <- 1 until (a - 1)) {
            val next = grouped(3).find(t => t.intersectsWith(current) && !used.contains(t)).get
            matrix(a - 1 - r)(0) = next
            used = used + next
            current = next
        }

        // inner parts
        for (r <- 1 until (a - 1)) {
            for (c <- 1 until (a - 1)) {
                val next = grouped(4).find(t =>
                    t.intersectsWith(matrix(r - 1)(c)) && t.intersectsWith(matrix(r)(c - 1)) && !used.contains(t)
                ).get
                matrix(r)(c) = next
                used = used + next
            }
        }

        // check
        for (r <- 1 until (a - 1)) {
            for (c <- 1 until (a - 1)) {
                val t = matrix(r)(c)
                val valid = t.intersectsWith(matrix(r - 1)(c)) &&
                    t.intersectsWith(matrix(r)(c - 1)) &&
                    t.intersectsWith(matrix(r)(c + 1)) &&
                    t.intersectsWith(matrix(r + 1)(c))
                if (!valid) {
                    throw new Error("invalid")
                }
            }
        }

        // Print matrix
        for (r <- 0 until a) {
            for (c <- 0 until a) {
                var t = matrix(r)(c)
                print(if (t != null) t.id.toString else "null")
                print(" ")
            }
            println()
        }

        // println("Variants test")
        // variants("abc\ndef\nghi".split("\n")).foreach(a => { a.foreach(l => println(l)); println() })

        val fixedMatrix: Array[Array[Tile]] = Array.fill(a)(Array.fill(a)(null))
        for (r <- 0 until a) {
            for (c <- 0 until a) {
                val top = if (r > 0) matrix(r - 1)(c) else null
                val right = if (c + 1 < a) matrix(r)(c + 1) else null
                val bottom = if (r + 1 < a) matrix(r + 1)(c) else null
                val left = if (c > 0) matrix(r)(c - 1) else null
                fixedMatrix(r)(c) = matrix(r)(c).getVariantForContext(top, right, bottom, left)
            }
        }

        val aa = matrix(0)(0).lines.length
        val img = Array.fill(a * (aa - 2))(".")

        // Print
        for (r <- 0 until a) {
            val tilesInRow = fixedMatrix(r)
            for (rr <- 0 until tilesInRow(0).lines.length) {
                tilesInRow.foreach(t => {
                    if (rr > 0 && rr < aa - 1) {
                        img(r * (aa - 2) + rr - 1) += t.lines(rr).substring(1, aa - 1)
                    }
                    print(t.lines(rr))
                    print(" ")
                })
                println()
            }
            println()
        }

        println()
        img.foreach(l => println(l))

        val monster = Array(
            "                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   ",
        )

        variants(monster).foreach(v => {
            v.foreach(l => println(l))
            println()
            println("-------------")
            println()
        })

        var imgArr = img.map(s => s.toArray)
        var count = 0
        variants(monster).foreach(v => {
            for (r <- 0 until (img.length - (v.length - 1))) {
                for (c <- 0 until (img(r).length - (v(0).length - 1))) {
                    var found = true
                    for (vr <- 0 until v.length) {
                        for (vc <- 0 until v(0).length) {
                            if (v(vr).charAt(vc) == '#' && img(r + vr).charAt(c + vc) != '#') {
                                found = false
                            }
                        }
                    }
                    if (found) {
                        count += 1
                        for (vr <- 0 until v.length) {
                            for (vc <- 0 until v(0).length) {
                                if (v(vr).charAt(vc) == '#') {
                                    imgArr(r + vr)(c + vc) = 'O'
                                }
                            }
                        }
                    }
                }
            }
        })
        println(count)
        var res = 0
        for (r <- 0 until img.length) {
            for (c <- 0 until img(r).length) {
                print(imgArr(r)(c))
                if (imgArr(r)(c)=='#') {
                    res += 1
                }
            }
            println()
        }
        println(res) // 1960 too low
    }
}
