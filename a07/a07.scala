import scala.io.StdIn
import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set

object main {
    val outerR = """(\w+ \w+) bags contain (.+).""".r
    val innerR = """\s*(\d+) (\w+ \w+) bags?\s*""".r
    val noOther = """\s*no other bags""".r

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        s1(lines)
        println("----------")
        s2(lines)
    }

    def s1(lines: List[String]): Unit = {
        var map = Map[String, List[String]]()
        lines.foreach(line => {
            line match {
                case outerR(bag, info) => {
                    val contained = info.split(",").toList
                    contained.foreach(c => {
                        c match {
                            case innerR(count, b) => {
                                map = map + (b -> (bag +: map.getOrElse(b, List())))
                            }
                            case noOther() => {
                            }
                            case _ => {
                                throw new Error("Invalid bag info: " + c)
                            }
                        }
                    })
                }
                case _ => {
                    println("nothing " + line)
                }
            }
        })
        var set = Set[String]()
        var stack = List("shiny gold")
        while (stack.size > 0) {
            println(stack)
            val current = stack.head
            val containing = map.getOrElse(current, List())
            set = set ++ containing
            stack = stack.tail ++ containing
        }
        println(stack)
        // println(map)
        println(set)
        println(set.size)
    }

    def s2(lines: List[String]): Unit = {
        var map = Map[String, List[(Int, String)]]()
        lines.foreach(line => {
            line match {
                case outerR(bag, info) => {
                    val contained = info.split(",").toList
                    val value: List[(Int, String)] = contained.map(c => {
                        c match {
                            case innerR(count, b) => {
                                Some((count.toInt, b))
                            }
                            case noOther() => {
                                None
                            }
                            case _ => {
                                throw new Error("Invalid bag info: " + c)
                            }
                        }
                    }).filter(opt => opt.isDefined).map(opt => opt.get)
                    map = map.updated(bag, value)
                }
                case _ => {
                    println("nothing " + line)
                }
            }
        })
        println(map)
        def count(bag: String): Long = {
            println("counting", bag)
            val inside = map.getOrElse(bag, List())
            1 + inside.map({ case (cnt, bg) => cnt * count(bg) }).sum
        }
        val res = count("shiny gold")
        println(res - 1)
    }
}
