import scala.io.StdIn
import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.immutable.Map

object main {
    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        val groups = lines.mkString("\n").split("\n\n").toList
        val counts = groups.map(countGroup)
        println(counts.sum)

        val intersectCounts = groups.map(countGroupIntersect)
        println(intersectCounts.sum)
    }

    def countGroup(str: String): Long = {
        var set = Set[Char]()
        """\s""".r.replaceAllIn(str, "").foreach(c => { set += c  })
        set.size
    }

    def countGroupIntersect(str: String): Long = {
        val sets = str.lines.map(line => {
            var set = Set[Char]()
            """\s""".r.replaceAllIn(line, "").foreach(c => { set += c  })
            set
        })
        val intersection = sets.reduce((a, b) => a.intersect(b))
        intersection.size
    }
}
