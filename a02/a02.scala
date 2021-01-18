import scala.io.StdIn
import scala.collection.immutable.List

object main {
    def main(args: Array[String]): Unit = {
        val spec = """(\d+)-(\d+) (\w): (\w+)""".r
        var sum = 0
        val lines = io.Source.stdin.getLines().toList
        for (ln <- lines) {
            val add = ln match {
                case spec(fromStr, toStr, charStr, pass) => {
                    val from = fromStr.toInt
                    val to = toStr.toInt
                    val char = charStr.charAt(0)
                    val count = pass.filter(c => c == char).length
                    // println(s"$from $to $char $pass $count")
                    if (from <= count && count <= to) 1 else 0
                }
                case _ => 0
            }
            sum += add
        }
        println(sum)

        println("--------------------------")
        sum = 0
        for (ln <- lines) {
            val add = ln match {
                case spec(fromStr, toStr, charStr, pass) => {
                    val i1 = fromStr.toInt
                    val i2 = toStr.toInt
                    val char = charStr.charAt(0)
                    println(s"$i1 $i2 $char $pass ${pass.charAt(i1 - 1)} ${pass.charAt(i2 - 1)}")
                    if (
                        (pass.charAt(i1 - 1) == char && pass.charAt(i2 - 1) != char)
                        || (pass.charAt(i2 - 1) == char && pass.charAt(i1 - 1) != char)
                        ) 
                        1
                    else
                        0
                }
                case _ => 0
            }
            sum += add
        }
        println(sum)
    }
}
